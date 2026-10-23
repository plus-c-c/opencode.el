;;; opencode.el --- Emacs interface to opencode -*- lexical-binding: t; byte-compile-warnings: (not docstrings-wide); -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: tools, llm, opencode
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "31.1") (magit "4.0") (plz "0.9") (plz-media-type "0.2.4") (plz-event-source "0.1.3"))
;; URL: https://codeberg.org/sczi/opencode.el/

;;; Commentary:

;; Emacs interface to opencode.
;; Provides a comint-based mode for interacting with an opencode server.

;;; Code:

(require 'json)
(require 'magit)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-diff-parser)
(require 'opencode-permission)
(require 'opencode-question)
(require 'opencode-sessions)
(require 'opencode-events)
(require 'opencode-filehooks)
(require 'opencode-theme)
(require 'opencode-sidebar)
(require 'opencode-ui)
(require 'opencode-diagnostics)
(require 'opencode-session-extra)
(require 'plz-media-type)
(require 'plz-event-source)
(require 'project)

;; pending fix upstream at https://github.com/r0man/plz-event-source/pull/15
;; see also: https://codeberg.org/sczi/opencode.el/issues/12
(setq plz-event-source-parser--line-regexp
      (rx (*? not-newline) (or "\r\n" "\n" "\r")))

(defgroup opencode nil
  "Emacs interface to opencode."
  :group 'applications)

(defgroup opencode-faces nil
  "Faces for opencode interface."
  :group 'opencode)

(defcustom opencode-host "localhost"
  "Hostname for the opencode server."
  :type 'string
  :group 'opencode)

(defcustom opencode-port 4096
  "Port for the opencode server."
  :type 'integer
  :group 'opencode)

(defcustom opencode-use-service-discovery t
  "Whether to connect to an existing OpenCode background service.
When non-nil, consult `opencode-service-file' before trying
`opencode-host' and `opencode-port'.  This is the normal way to find
the V2 server started by the OpenCode CLI."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-service-file
  (expand-file-name "~/.local/state/opencode/service.json")
  "OpenCode service registration file."
  :type 'file
  :group 'opencode)

(defcustom opencode-command "opencode"
  "Base command for the opencode executable.
Used when `opencode-serve-command' is nil to construct the serve command."
  :type 'string
  :group 'opencode)

(defcustom opencode-serve-command nil
  "Full command to start the opencode server.
When nil, the command is constructed from `opencode-command',
`opencode-host', and `opencode-port'.

Set this to a custom command for special cases like nix:
  \"nix run github:numtide/nix-ai-tools#opencode -- serve --port 4096 --hostname localhost\""
  :type '(choice (const :tag "Construct from opencode-command" nil)
          (string :tag "Custom command"))
  :group 'opencode)

(defcustom opencode-auto-start-server t
  "Whether to automatically start a server if none is running.
When nil, `opencode' will only connect to an already running server."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-session-deleted-functions nil
  "Hook run when an opencode session is deleted.
This is an abnormal hook. Each function receives one argument, SESSION-ID."
  :type 'hook
  :group 'opencode)

(defvar opencode--process nil
  "Opencode server process when started by Emacs.")

;; so invisible prompt "> " doesn't make whole prompt invisible
(add-to-list 'comint--prompt-rear-nonsticky 'invisible)

(defun opencode--ensure-local-noproxy (url)
  "Make sure local URL bypasses an HTTP proxy in plz/curl."
  (when (and (stringp url)
             (string-match-p "\\`https?://\\(localhost\\|127\\.0\\.0\\.1\\|\\[::1\\]\\)" url))
    ;; curl uses the last --noproxy option, so remove any earlier ones and
    ;; put our value at the end.
    (let (cleaned skip)
      (dolist (arg plz-curl-default-args)
        (cond
         (skip (setq skip nil))
         ((equal arg "--noproxy") (setq skip t))
         (t (push arg cleaned))))
      (setq plz-curl-default-args
            (append (nreverse cleaned)
                    '("--noproxy" "localhost,127.0.0.1,::1"))))))

(defun opencode--read-service-file ()
  "Return the parsed OpenCode service registration, or nil."
  (when (file-readable-p opencode-service-file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents opencode-service-file)
          (goto-char (point-min))
          (json-parse-buffer :object-type 'alist))
      (error nil))))

(defun opencode--api-url-running-p (url &optional password)
  "Return the server info when a V2 server is healthy at URL.
PASSWORD, when non-nil, is used for Basic authentication."
  (opencode--ensure-local-noproxy url)
  (condition-case err
      (let* ((opencode-server-password (or password opencode-server-password))
             (result (plz 'get (concat (string-remove-suffix "/" url) "/api/info")
                       :headers (delq nil (list (opencode--auth-header)))
                       :as (lambda ()
                             (goto-char (point-min))
                             (unless (string-empty-p (buffer-string))
                               (json-parse-buffer :object-type 'alist)))
                       :timeout 1)))
        (when (and (listp result)
                   (stringp (alist-get 'version result))
                   (string-prefix-p "2." (alist-get 'version result)))
          result))
    (plz-http-error
     (when-let (plz-error (cl-third err))
       (when (= 401 (plz-response-status (plz-error-response plz-error)))
         (user-error "OpenCode server is running but requires authentication: \
set `opencode-server-password' or use service discovery"))))
    (error nil)))

(defun opencode--server-running-p ()
  "Return non-nil if a compatible OpenCode server is running at configured host and port."
  (opencode--api-url-running-p
   (format "http://%s:%d" opencode-host opencode-port)))

(defun opencode--service-info ()
  "Return healthy V2 service information from `opencode-service-file'."
  (when opencode-use-service-discovery
    (when-let* ((info (opencode--read-service-file))
                (url (alist-get 'url info))
                (password (alist-get 'password info)))
      (when (opencode--api-url-running-p url password)
        info))))

(defun opencode--serve-command ()
  "Return the command to start the opencode server."
  (or opencode-serve-command
      (format "%s serve --port %d --hostname %s"
              opencode-command opencode-port opencode-host)))

(defun opencode--start-server (on-connect)
  "Start an opencode server and run ON-CONNECT when ready."
  (setf opencode-server-password
        (or opencode-server-password
            (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
              (apply #'string
                     (cl-loop repeat 32
                              collect (aref chars (random (length chars))))))))
  (setf opencode--process
        (let ((process-environment (cl-list*
                                    (format "OPENCODE_SERVER_USERNAME=%s"
                                            opencode-server-username)
                                    (format "OPENCODE_SERVER_PASSWORD=%s"
                                            opencode-server-password)
                                    process-environment)))
          (start-process-shell-command
           "opencode" "*opencode-serve*"
           (opencode--serve-command))))

  (set-process-filter
   opencode--process
   (lambda (process output)
     (with-current-buffer "*opencode-serve*"
       (goto-char (process-mark process))
       (insert output))
     (when (string-match-p "server listening on" output)
       ;; Remove filter to avoid repeated callbacks
       (set-process-filter process nil)
       (opencode-connect opencode-host opencode-port on-connect))))

  (set-process-sentinel
   opencode--process
   (lambda (process event)
     (when (memq (process-status process) '(exit signal))
       (unless (zerop (process-exit-status process))
         (pop-to-buffer (process-buffer process))
         (error "OpenCode failed, %s" (string-trim-right event "\n")))))))

(defun opencode--service-password-for-url (url)
  "Return the service password for URL, if it is registered."
  (when-let* ((info (opencode--read-service-file))
              (service-url (alist-get 'url info)))
    (when (equal (string-remove-suffix "/" service-url)
                 (string-remove-suffix "/" url))
      (alist-get 'password info))))

(defun opencode-connect-url (url on-connect &optional password)
  "Connect to the OpenCode server at URL and call ON-CONNECT.
PASSWORD is used for Basic authentication when non-nil."
  (when opencode--event-subscription
    (user-error "Already connected"))
  (opencode--ensure-local-noproxy url)
  (setq opencode-api-url (string-remove-suffix "/" url)
        opencode-server-password (or password
                                      opencode-server-password
                                      (opencode--service-password-for-url url))
        opencode--slash-commands-by-directory nil)
  (opencode-api-info info
    (setq opencode-api-version (alist-get 'version info))
    (opencode--subscribe-global-events)
    (opencode-api-configured-providers result
      (setq opencode-providers (alist-get 'providers result))
      (opencode--fetch-agents on-connect))
    (message "Connected to %s (OpenCode %s)"
             opencode-api-url opencode-api-version)))

(defun opencode-autoconnect (on-connect)
  "Connect to an existing server if one is running, otherwise start a new one.
Only starts new one if `opencode-auto-start-server' is non-nil.
Run ON-CONNECT after connected."
  (cond
   ;; Already connected
   (opencode--event-subscription
    (funcall on-connect))
   ;; We started a server process that's still alive
   ((process-live-p opencode--process)
    (funcall on-connect))
   ;; A V2 background service is registered and healthy
   ((opencode--service-info)
    (let ((info (opencode--service-info)))
      (opencode-connect-url (alist-get 'url info)
                            on-connect
                            (alist-get 'password info))))
   ;; Server already running externally at the configured address
   ((opencode--server-running-p)
    (opencode-connect opencode-host opencode-port on-connect))
   ;; Need to start a new server
   (opencode-auto-start-server
    (opencode--start-server on-connect))
   ;; Auto-start disabled, no server running
   (t
    (user-error "No opencode server running at %s:%d (auto-start disabled)"
                opencode-host opencode-port))))

;;;###autoload
(defun opencode ()
  "Open opencode sessions control buffer for the current project directory.
Connects to an existing server if one is running, otherwise starts a new one
if `opencode-auto-start-server' is non-nil."
  (interactive)
  (let ((project-dir (expand-file-name
                      (if-let (proj (project-current))
                          (project-root proj)
                        default-directory))))
    (opencode-autoconnect (lambda () (opencode-open-project project-dir)))))

(defun opencode-connect (host port on-connect)
  "Connect to opencode server, prompting for HOST and PORT.
Call ON-CONNECT once connected."
  (interactive
   (list (read-string "Host: " opencode-host)
         (read-number "Port: " opencode-port)))
  (opencode-connect-url (format "http://%s:%d" host port) on-connect))

(defun opencode-open-project (directory)
  "Open sessions control buffer for DIRECTORY."
  (opencode--download-slash-commands directory)
  (let ((buffer-name (format "*OpenCode Sessions in %s*" directory)))
    (unless (get-buffer buffer-name)
      (with-current-buffer (get-buffer-create buffer-name)
        (opencode-session-control-mode)
        (setq default-directory directory)
        (opencode-api-current-project project
          (let-alist project
            (setf (map-elt opencode--session-control-buffers .id)
                  (cons (current-buffer)
                        (seq-filter #'buffer-live-p
                                    (map-elt opencode--session-control-buffers
                                             .id))))))
        (opencode-sessions-redisplay)))
    (pop-to-buffer buffer-name)))

(defvar opencode-worktree-directory (expand-file-name "~/opencode_worktrees/")
  "Directory to store worktrees created for opencode.")

(defun opencode-new-worktree ()
  "Create a new git branch, and worktree prompting for a name.
Then open an opencode session in it."
  (interactive)
  (let* ((name (read-string "Worktree and branch name: "))
         (directory (file-name-concat opencode-worktree-directory name)))
    (when (magit-worktree-branch directory name "HEAD")
      (let ((default-directory directory))
        (opencode-new-session)))))

(defun opencode-select-project ()
  "Completing read to prompt which project to select."
  (interactive)
  (opencode-autoconnect
   (lambda ()
     (opencode-api-projects projects
       (opencode-open-project
        (opencode--annotated-completion
         "Project: "
         (cl-loop for project in projects
                  for worktree = (alist-get 'worktree project)
                  when worktree
                  collect (list (string-remove-prefix
                                 (expand-file-name "~/")
                                 worktree)
                                worktree
                                (opencode--format-time-ago
                                 (opencode--time-ago
                                  project 'updated))))))))))

(defun opencode--selection-change-hook (&optional _frame)
  "Hook to remove session from the alerted sessions list when it's visited.
Also prompts for pending questions or permissions if any."
  (when opencode-session-id
    (setf opencode-alerted-sessions
          (cl-delete-if (lambda (session)
                          (string= opencode-session-id
                                   (alist-get 'id session)))
                        opencode-alerted-sessions)
          opencode-last-session-buffer (current-buffer))
    ;; Handle pending questions when buffer becomes active
    (when opencode-session-pending-questions
      (let ((pending opencode-session-pending-questions))
        (setq opencode-session-pending-questions nil)
        (opencode--prompt-questions (car pending) (cdr pending))))
    ;; Handle pending permissions when buffer becomes active
    (when opencode-session-pending-permission
      (run-at-time 0 nil #'opencode-respond-permission))))

(add-hook 'window-selection-change-functions 'opencode--selection-change-hook)

;; Handles the case where the session buffer was already selected when the
;; request arrived but Emacs did not have OS focus at the time.
(add-function :after after-focus-change-function #'opencode--selection-change-hook)

;;; V2 event handling

(defun opencode-disconnect (&optional event)
  "Disconnect from opencode server, optionally log EVENT."
  (interactive)
  (opencode--log-event "DISCONNECT" event)
  (when (process-live-p opencode--event-subscription)
    (kill-process opencode--event-subscription))
  (when (process-live-p opencode--process)
    (set-process-sentinel opencode--process nil)
    (kill-process opencode--process))
  (setq opencode--event-subscription nil
        opencode--slash-commands-by-directory nil)
  (clrhash opencode--v2-tool-calls)
  (clrhash opencode--last-idle-handled))

(defun opencode--fetch-agents (callback)
  "Fetch available agents from server and filter out hidden agents.
Run CALLBACK once fetched."
  (opencode-api-agents agents
    (setq opencode-agents
          (seq-filter (lambda (agent)
                        (opencode--json-falsy (alist-get 'hidden agent)))
                      agents))
    (funcall callback)))

(cl-defun opencode-new-session (&key title callback)
  "Create a new session. With a prefix argument it will ask for TITLE.
Without it will use a default title and then automatically generate one.
If CALLBACK is given, it will be called with the session after it is created."
  (interactive
   (when current-prefix-arg
     (list :title (read-string "Title: "))))
  (opencode-autoconnect
   (lambda ()
     (opencode--download-slash-commands default-directory)
     (opencode-api-create-session
         (append (when title `((title . ,title)))
                 `((location . ((directory . ,(directory-file-name
                                               (expand-file-name default-directory)))))))
         session
       (opencode-open-session session :callback callback)))))

(defun opencode-toggle-mcp ()
  "Completing read to select an MCP to toggle."
  (interactive)
  (opencode-api-mcps mcps
    (let ((mcp (opencode--annotated-completion
                "MCP: "
                (cl-loop for mcp in mcps
                         for (mcp-name . mcp-info) = mcp
                         collect (list (symbol-name mcp-name)
                                       mcp-name
                                       (pcase (alist-get 'status mcp-info)
                                         ("connected" "🟢 connected")
                                         ("disabled" "🔴 disabled")
                                         ("pending" "🟡 pending")
                                         ("needs_auth" "🔑 needs auth")
                                         ("failed" "❌ failed")
                                         (_ "⚪ unknown")))))))
      (pcase (map-nested-elt mcps `(,mcp status))
        ("connected" (opencode-api-disable-mcp (mcp)
                         _res
                       (message "Disabled %s" mcp)))
        ((or "disabled" "failed" "needs_auth")
         (opencode-api-enable-mcp (mcp)
             _res
           (message "Enabled %s" mcp)))
        (_ (message "MCP %s is not in a toggleable state" mcp))))))

(defun opencode-fork-session ()
  "Fork the current session from the message at point.
Creates a new session starting from the current user message.
If point is before the first prompt, creates a new session instead."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an opencode session buffer"))
  (opencode--current-message-id message-id
    (if message-id
        (opencode-api-fork-session (opencode-session-id)
            `((before . ,message-id))
            session
          (opencode-open-session-same-window session))
      ;; if before the first prompt just open a new session
      (opencode-new-session))))

(defun opencode-revert-message ()
  "Select a message to revert in the current session."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an opencode session buffer"))
  (opencode--current-message-id message-id
    (if message-id
        (opencode-api-revert-message (opencode-session-id)
            `((messageID . ,message-id) (files . t))
            result
          (if result
              (opencode-api-commit-revert (opencode-session-id)
                  _res
                (message "Reverted edits after message"))
            (message "Failed to revert message")))
      (user-error "No user message at point"))))

(defun opencode-delete-message ()
  "Delete the message at point from the current session."
  (interactive)
  (user-error "OpenCode V2 does not support deleting a single message; \
use revert or fork instead"))

(defun opencode-unrevert-all ()
  "Clear a staged revert in the current session."
  (interactive)
  (opencode-api-unrevert-all (opencode-session-id)
      _result
    (message "Cleared staged revert")))

(defun opencode--download-slash-commands (directory)
  "Download slash commands for DIRECTORY."
  (setf directory (opencode--normalize-directory directory))
  (unless (assoc directory opencode--slash-commands-by-directory)
    (let ((default-directory directory))
      (opencode-api-commands commands
        (setf (alist-get directory opencode--slash-commands-by-directory
                         nil nil #'string=)
              commands)))))

(provide 'opencode)
;;; opencode.el ends here
