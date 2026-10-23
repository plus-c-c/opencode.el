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

(defvar opencode-event-log-max-lines nil
  "Maximum number of lines to log in the opencode event log buffer.
Or nil to disable logging.")

(defun opencode--log-event (type event)
  "Log EVENT of TYPE to the opencode log buffer."
  (when opencode-event-log-max-lines
    (with-current-buffer (get-buffer-create "*opencode-event-log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "[%s] %s: %s\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S")
                        type
                        event))
        (opencode--truncate-at-max-lines opencode-event-log-max-lines)))))

(defun opencode--maybe-run-file-edited-hook (part)
  "Record edited files and run per-file hooks if PART completed a file edit."
  (let-alist part
    (when (and (equal .type "tool")
               (equal .state.status "completed"))
      (dolist (file-ranges
               (pcase .tool
                 ("write"
                  (when .state.input.filePath
                    (list (cons .state.input.filePath nil))))
                 ((or "edit" "apply_patch")
                  (unless .state.metadata.diff
                    (error "Missing diff metadata for %s tool" .tool))
                  (opencode-diff->source-line-ranges .state.metadata.diff))))
        (when-let ((file (opencode--resolve-file-reference (car file-ranges))))
          (opencode--with-session-buffer .sessionID
            (cl-pushnew file opencode--files-edited-this-turn :test #'equal))
          (run-hook-with-args 'opencode-file-edited-functions
                              file (cdr file-ranges)))))))

(defvar opencode--files-finished-editing-current nil
  "Current `opencode-files-finished-editing-functions' run context.")

(defvar opencode--files-finished-editing-queue nil
  "Queued `opencode-files-finished-editing-functions' run contexts.")

(defun opencode-report-diagnostic (message)
  "Queue diagnostic MESSAGE for the active finished-editing hook."
  (unless opencode--files-finished-editing-current
    (error "`opencode-report-diagnostic' called outside a finished-editing hook"))
  (when (and message (not (string= message "")))
    (push message
          (plist-get opencode--files-finished-editing-current :diagnostics))))

(defun opencode-hook-finished ()
  "Mark the active finished-editing hook as complete."
  (let ((context opencode--files-finished-editing-current))
    (unless context
      (error "`opencode-hook-finished' called outside a finished-editing hook"))
    (unless (plist-get context :active-hook)
      (error "`opencode-hook-finished' called without an active hook"))
    (plist-put context :active-hook nil)
    (opencode--run-next-files-finished-editing-hook)))

(defun opencode--files-finished-editing-diagnostic-message (diagnostics)
  "Return a synthetic input message for DIAGNOSTICS."
  (concat
   "Diagnostics were reported after editing files. "
   "Please fix any problems caused by your changes.\n\n"
   (mapconcat #'identity diagnostics "\n\n")))

(defun opencode--maybe-start-files-finished-editing-hook ()
  "Start the next queued finished-editing hook run if none is active."
  (unless opencode--files-finished-editing-current
    (when opencode--files-finished-editing-queue
      (let ((context (pop opencode--files-finished-editing-queue)))
        (setq opencode--files-finished-editing-current context)
        (opencode--run-next-files-finished-editing-hook)))))

(defun opencode--finish-files-finished-editing-hook-run (context)
  "Finish finished-editing hook run CONTEXT and send queued diagnostics."
  (let ((diagnostics (nreverse (plist-get context :diagnostics)))
        (buffer (plist-get context :session-buffer)))
    (setq opencode--files-finished-editing-current nil)
    (when (and diagnostics (buffer-live-p buffer))
      (with-current-buffer buffer
        (opencode-session--send-synthetic-input
         (opencode--files-finished-editing-diagnostic-message diagnostics)))))
  (opencode--maybe-start-files-finished-editing-hook))

(defun opencode--run-next-files-finished-editing-hook ()
  "Run the next hook in the active finished-editing context."
  (let ((context opencode--files-finished-editing-current))
    (unless context
      (error "No active finished-editing hook context"))
    (when-let ((buffer (plist-get context :session-buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (if-let ((hook (car (plist-get context :hooks))))
              (let (result errored)
                (plist-put context :hooks (cdr (plist-get context :hooks)))
                (plist-put context :active-hook hook)
                (condition-case err
                    (setq result (funcall hook (plist-get context :files)))
                  (error
                   (setq errored t)
                   (opencode--log-event
                    "WARNING FINISHED EDITING HOOK"
                    (format "%S failed: %s" hook (error-message-string err)))
                   (when (and (eq opencode--files-finished-editing-current context)
                              (eq (plist-get context :active-hook) hook))
                     (plist-put context :active-hook nil)
                     (opencode--run-next-files-finished-editing-hook))))
                (when (and (not errored)
                           (not (eq result :opencode-async))
                           (eq opencode--files-finished-editing-current context)
                           (eq (plist-get context :active-hook) hook))
                  (plist-put context :active-hook nil)
                  (opencode--run-next-files-finished-editing-hook)))
            (opencode--finish-files-finished-editing-hook-run context)))))))

(defun opencode--maybe-run-files-finished-editing-hook (session-id)
  "Run `opencode-files-finished-editing-functions' for SESSION-ID's pending files."
  (opencode--with-session-buffer session-id
    (when opencode--files-edited-this-turn
      (let ((files opencode--files-edited-this-turn))
        (setq opencode--files-edited-this-turn nil)
        (when-let ((hooks (append opencode-files-finished-editing-functions
                                  opencode-project-files-finished-editing-functions)))
          (setq opencode--files-finished-editing-queue
                (append opencode--files-finished-editing-queue
                        (list (list :session-buffer (current-buffer)
                                    :files files
                                    :hooks hooks
                                    :diagnostics nil
                                    :active-hook nil))))
          (opencode--maybe-start-files-finished-editing-hook))))))

(defun opencode--indent-files (files)
  "Indent FILES with Emacs."
  (dolist (file files)
    (with-current-buffer (find-file-noselect file)
      (unless (opencode--indent-sensitive-buffer-p)
        (let ((inhibit-message t))
          (revert-buffer t t t)
          (indent-region (point-min) (point-max)))
        (when (buffer-modified-p)
          (message "opencode indented %s" file)
          (save-buffer))))))

(defun opencode-run-command-diagnostic (command)
  "Run COMMAND asynchronously and report diagnostics.
For use within `opencode-files-finished-editing-functions' or
`opencode-project-files-finished-editing-functions'."
  (let* ((command-name (car command))
         (buffer (generate-new-buffer (format "*%s*" command-name)))
         (command-string (string-join command " ")))
    (condition-case err
        (progn
          (make-process
           :name command-name
           :buffer buffer
           :command command
           :connection-type 'pipe
           :noquery t
           :sentinel
           (lambda (process _event)
             (when (memq (process-status process) '(exit signal))
               (unwind-protect
                   (unless (zerop (process-exit-status process))
                     (let ((output (with-current-buffer (process-buffer process)
                                     (string-trim (buffer-string)))))
                       (opencode-report-diagnostic
                        (if (equal output "")
                            (format "`%s` failed" command-string)
                          (format "`%s` failed:\n\n%s"
                                  command-string
                                  output)))))
                 (kill-buffer buffer)
                 (opencode-hook-finished)))))
          :opencode-async)
      (error
       (kill-buffer buffer)
       (opencode-report-diagnostic
        (format "Failed to start `%s`: %s"
                command-string
                (error-message-string err)))
       nil))))

(defun opencode--emacs-ert-command ()
  "Return the batch Emacs command used to test with ERT."
  (append
   (list (concat invocation-directory invocation-name) "--batch")
   (cl-loop for path in (delete-dups (append (list default-directory) load-path nil))
            when (and (stringp path) (file-directory-p path))
            append (list "-L" (expand-file-name path)))
   (cl-loop for file in (directory-files-recursively default-directory
                                                     "\\(?:-test\\|-tests\\)\\.el\\'")
            append (list "-l" file))
   (list "-f" "ert-run-tests-batch-and-exit")))

(defun opencode-uv-pytest (_files)
  "Run pytest with uv and report diagnostics."
  (opencode-run-command-diagnostic
   '("uv" "run" "python3" "-m" "pytest")))

(defun opencode-emacs-ert (_files)
  "Run ERT and report diagnostics."
  (opencode-run-command-diagnostic
   (opencode--emacs-ert-command)))

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

(defvar opencode--v2-tool-calls (make-hash-table :test 'equal)
  "Tool calls seen by the V2 event bridge, keyed by call id.")

(defvar opencode--last-idle-handled (make-hash-table :test 'equal)
  "Last time an idle event was handled for a session.")

(defun opencode--error-message (error)
  "Return a human-readable message from ERROR."
  (or (and (listp error) (alist-get 'message error))
      (and (listp error)
           (alist-get 'message (alist-get 'data error)))
      (format "%s" error)))

(defun opencode--event-session-id (data)
  "Return the session id carried by event DATA."
  (or (alist-get 'sessionID data)
      (alist-get 'sessionID (alist-get 'form data))
      (alist-get 'id (alist-get 'info data))
      (alist-get 'id data)))

(defun opencode--event-session-directory (data)
  "Return the default directory of the session in event DATA."
  (when-let* ((session-id (opencode--event-session-id data))
              (buffer (gethash session-id opencode-session-buffers)))
    (when (buffer-live-p buffer)
      (buffer-local-value 'default-directory buffer))))

(defun opencode--redisplay-control-buffers (&optional project-id)
  "Refresh session control buffers, optionally only for PROJECT-ID."
  (let ((redisplay
         (lambda (buffers)
           (dolist (buffer buffers)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (ignore-errors (opencode-sessions-redisplay))))))))
    (if project-id
        (funcall redisplay
                 (map-elt opencode--session-control-buffers project-id))
      (maphash (lambda (_project buffers) (funcall redisplay buffers))
               opencode--session-control-buffers))))

(defun opencode--session-idle (session-id)
  "Handle the end of a turn for SESSION-ID."
  (let ((now (float-time))
        (last (gethash session-id opencode--last-idle-handled)))
    ;; V2 may report both session.status and an execution event.
    (unless (and last (< (- now last) 1.0))
      (puthash session-id now opencode--last-idle-handled)
      (unless (opencode--defer-files-finished-editing-hook session-id)
        (opencode--maybe-run-files-finished-editing-hook session-id))
      (opencode--with-session-buffer session-id
        (setq opencode-session-status "idle")
        (force-mode-line-update)
        (opencode--show-prompt)
        (unless (opencode--buffer-active-p (current-buffer))
          (opencode-api-session (session-id)
              session
            (unless (alist-get 'parentID session)
              (opencode--toast-show
               `((title . "OpenCode Finished")
                 (message . ,(alist-get 'title session))
                 (variant . "success")))
              (push session opencode-alerted-sessions))))))))

(defun opencode--handle-session-status (data)
  "Handle a V2 session.status event DATA."
  (let* ((session-id (alist-get 'sessionID data))
         (status (alist-get 'status data))
         (type (opencode-v2--status status)))
    (pcase type
      ("busy" (opencode-session--set-status session-id "busy"))
      ("idle" (opencode--session-idle session-id))
      ("retry"
       (opencode--toast-show
        `((title . "OpenCode Retrying")
          (message . ,(format "%s\n\nRetry #%d"
                              (or (alist-get 'message status) "")
                              (or (alist-get 'attempt status) 0)))
          (variant . "warning")))))))

(defun opencode--handle-session-event (type data)
  "Handle session lifecycle event TYPE with DATA."
  (let ((session-id (opencode--event-session-id data)))
    (pcase type
      ((or "session.created" "session.updated")
       (let* ((info (or (alist-get 'info data) data))
              (project-id (alist-get 'projectID info)))
         (opencode--redisplay-control-buffers project-id)
         (when (and session-id (equal type "session.updated"))
           (let ((title (alist-get 'title info)))
             (opencode--with-session-buffer session-id
               (when title
                 (rename-buffer (format "*OpenCode: %s*" title) t)))))))
      ("session.deleted"
       (when session-id
         (run-hook-with-args 'opencode-session-deleted-functions session-id)
         (opencode--with-session-buffer session-id
           (when-let ((process (get-buffer-process (current-buffer))))
             (delete-process process)))
         (remhash session-id opencode-session-buffers)
         (opencode--redisplay-control-buffers)))
      ("session.status"
       (opencode--handle-session-status data))
      ("session.error"
       (when session-id
         (opencode-session--display-error
          session-id (opencode--error-message (alist-get 'error data)))))
      ("session.usage.updated" nil)
      ((or "session.idle"
           "session.execution.succeeded"
           "session.execution.interrupted")
       (when session-id (opencode--session-idle session-id)))
      ("session.execution.failed"
       (when session-id
         (let ((error (alist-get 'error data)))
           (when error
             (opencode-session--display-error
              session-id (opencode--error-message error))))
         (opencode--session-idle session-id)))
      ((or "session.retried" "session.next.retried")
       (opencode--toast-show
        `((title . "OpenCode Retrying")
          (message . ,(format "Retry #%d: %s"
                              (or (alist-get 'attempt data) 0)
                              (opencode--error-message
                               (alist-get 'error data))))
          (variant . "warning"))))
      ("session.name" nil))))

(defun opencode--handle-step-event (type data)
  "Handle a V2 step event TYPE with DATA."
  (let ((session-id (alist-get 'sessionID data))
        (message-id (alist-get 'assistantMessageID data)))
    (when (and session-id message-id)
      (pcase type
        ("session.step.started"
         (opencode-session--message-updated
          `((id . ,message-id)
            (sessionID . ,session-id)
            (role . "assistant")
            (agent . ,(alist-get 'agent data))
            (model . ,(alist-get 'model data))
            (tokens . ((input . 0) (output . 0) (reasoning . 0)
                       (cache . ((read . 0) (write . 0)))))
            (time . ((created . ,(or (alist-get 'started data)
                                     (float-time))))))))
        ("session.step.ended"
         (opencode-session--message-updated
          `((id . ,message-id)
            (sessionID . ,session-id)
            (role . "assistant")
            (tokens . ,(or (alist-get 'tokens data) '()))
            (finish . ,(alist-get 'finish data))
            (time . ((completed . ,(float-time)))))))
        ("session.step.failed"
         (opencode-session--message-updated
          `((id . ,message-id)
            (sessionID . ,session-id)
            (role . "assistant")
            (finish . "error")
            (error . ,(alist-get 'error data))
            (time . ((completed . ,(float-time))))))
         (opencode-session--display-error
          session-id (opencode--error-message (alist-get 'error data))))))))

(defun opencode--handle-text-event (type data kind)
  "Handle a V2 text or reasoning event TYPE with DATA and KIND."
  (let* ((session-id (alist-get 'sessionID data))
         (message-id (alist-get 'assistantMessageID data))
         (ordinal (or (alist-get 'ordinal data) 0))
         (part-id (format "%s-%s-%s" message-id kind ordinal))
         (ended (string-suffix-p ".ended" type))
         (text (or (alist-get 'text data) ""))
         (delta (alist-get 'delta data))
         (part `((id . ,part-id)
                 (sessionID . ,session-id)
                 (messageID . ,message-id)
                 (type . ,kind)
                 (text . ,text)
                 (time . ,(when ended `((end . ,(float-time))))))))
    (opencode-session--update-part part-id part delta kind)))

(defun opencode--record-v2-tool-files (session-id name input output-paths)
  "Record and run hooks for files edited by tool NAME with INPUT.
OUTPUT-PATHS is the list of paths reported by the V2 tool result."
  (let ((files (delq nil (cons (opencode-v2--tool-file name input)
                               (append output-paths nil)))))
    (when files
      (opencode--with-session-buffer session-id
        (dolist (file files)
          (when-let ((path (opencode--resolve-file-reference file)))
            (unless (member path opencode--files-edited-this-turn)
              (cl-pushnew path opencode--files-edited-this-turn :test #'equal)
              (run-hook-with-args 'opencode-file-edited-functions
                                  path nil))))))))

(defun opencode--handle-tool-event (type data)
  "Handle a V2 tool event TYPE with DATA."
  (let* ((session-id (alist-get 'sessionID data))
         (message-id (alist-get 'assistantMessageID data))
         (call-id (alist-get 'id data))
         (call (gethash call-id opencode--v2-tool-calls))
         (name (or (plist-get call :name) (alist-get 'name data) "tool"))
         (input (or (plist-get call :input) (alist-get 'input data))))
    (pcase type
      ("session.tool.called"
       (puthash call-id
                (list :session session-id :message message-id
                      :name name :input input)
                opencode--v2-tool-calls)
       (let ((part `((id . ,call-id)
                     (sessionID . ,session-id)
                     (messageID . ,message-id)
                     (type . "tool")
                     (tool . ,name)
                     (callID . ,call-id)
                     (state . ((status . "running")
                               (input . ,input)
                               (metadata . ,(alist-get 'metadata data))))
                     (time . ((created . ,(float-time)))))))
         (opencode-session--update-part call-id part nil "tool")))
      ("session.tool.success"
       (let* ((output (opencode-v2--tool-output (alist-get 'content data)))
              (state `((status . "completed")
                       (input . ,input)
                       (content . ,(alist-get 'content data))
                       (metadata . ,(alist-get 'metadata data))
                       (output . ,output)))
              (part `((id . ,call-id)
                      (sessionID . ,session-id)
                      (messageID . ,message-id)
                      (type . "tool")
                      (tool . ,name)
                      (callID . ,call-id)
                      (state . ,state)
                      (time . ((end . ,(float-time)))))))
         (opencode-session--update-part call-id part nil "tool")
         (opencode--record-v2-tool-files
          session-id name input (alist-get 'outputPaths data))
         (remhash call-id opencode--v2-tool-calls)))
      ("session.tool.failed"
       (let ((error (opencode--error-message (alist-get 'error data))))
         (opencode-session--update-part
          call-id
          `((id . ,call-id)
            (sessionID . ,session-id)
            (messageID . ,message-id)
            (type . "tool")
            (tool . ,name)
            (callID . ,call-id)
            (state . ((status . "error")
                      (input . ,input)
                      (output . ,error)
                      (metadata . ,(alist-get 'metadata data))))
            (time . ((end . ,(float-time)))))
          nil "tool")
         (remhash call-id opencode--v2-tool-calls))))))

(defun opencode--handle-file-edited (data)
  "Handle a V2 file.edited event DATA."
  (let* ((file (alist-get 'file data))
         (session-id (or (alist-get 'sessionID data)
                         (and opencode-last-session-buffer
                              (buffer-local-value 'opencode-session-id
                                                  opencode-last-session-buffer)))))
    (when (and file session-id)
      (opencode--with-session-buffer session-id
        (when-let ((path (opencode--resolve-file-reference file)))
          (unless (member path opencode--files-edited-this-turn)
            (cl-pushnew path opencode--files-edited-this-turn :test #'equal)
            (run-hook-with-args 'opencode-file-edited-functions path nil)))))))

(defun opencode--handle-v2-event (type data)
  "Handle a V2 event TYPE with DATA."
  (cond
   ((string-prefix-p "session.text." type)
    (opencode--handle-text-event type data "text"))
   ((string-prefix-p "session.reasoning." type)
    (opencode--handle-text-event type data "reasoning"))
   ((string-prefix-p "session.tool." type)
    (opencode--handle-tool-event type data))
   ((string-prefix-p "session.step." type)
    (opencode--handle-step-event type data))
   ((member type '("session.created" "session.updated" "session.deleted"
                   "session.status" "session.error" "session.usage.updated"
                   "session.idle" "session.execution.succeeded"
                   "session.execution.failed" "session.execution.interrupted"
                   "session.retried" "session.next.retried"))
    (opencode--handle-session-event type data))
   ((equal type "permission.asked")
    (opencode--permission-request
     (alist-get 'id data)
     (alist-get 'sessionID data)
     (alist-get 'permission data)
     (alist-get 'metadata data)
     (alist-get 'patterns data)
     (alist-get 'always data)))
   ((equal type "permission.v2.asked")
    (opencode--permission-request
     (alist-get 'id data)
     (alist-get 'sessionID data)
     (alist-get 'action data)
     (alist-get 'metadata data)
     (alist-get 'resources data)
     (alist-get 'save data)))
   ((equal type "form.created")
    (when-let ((form (alist-get 'form data)))
      (opencode-form--handle form)))
   ((equal type "question.asked")
    (when (fboundp 'opencode-form--handle-question)
      (opencode-form--handle-question data)))
   ((equal type "file.edited")
    (opencode--handle-file-edited data))
   ((equal type "tui.toast.show")
    (opencode--toast-show data))
   ((member type '("session.diff" "file.watcher.updated"
                   "session.compacted" "session.tool.input.started"
                   "session.tool.input.delta" "session.tool.input.ended"
                   "session.tool.progress" "session.step.streamed"
                   "session.prompted" "session.next.prompted"
                   "session.next.context.updated" "form.replied"
                   "form.cancelled" "server.connected"))
    nil)
   (t
    (opencode--log-event "WARNING" (format "unhandled event %s" type)))))

(defun opencode--handle-message (event)
  "Handle a decoded V2 EVENT from the opencode server."
  (let ((type (alist-get 'type event))
        (data (alist-get 'data event)))
    (unless (member type '("file.watcher.updated" "session.diff"))
      (opencode--log-event "MESSAGE" event))
    (opencode--handle-v2-event type data)))

(defun opencode--handle-global-event (event)
  "Handle a V2 EVENT from opencode's global event stream."
  (let* ((data (json-read-from-string (plz-event-source-event-data event)))
         (event-data (alist-get 'data data))
         (directory (or (alist-get 'directory (alist-get 'location data))
                        (opencode--event-session-directory event-data))))
    (if directory
        (with-current-buffer (or (process-buffer opencode--event-subscription)
                                 (current-buffer))
          (let ((default-directory (opencode--normalize-directory directory)))
            (opencode--handle-message data)))
      (opencode--handle-message data))))

(defun opencode--subscribe-global-events ()
  "Subscribe to the OpenCode V2 event stream."
  (setq opencode--event-subscription
        (plz-media-type-request
          'get (concat opencode-api-url "/api/event")
          :as `(media-types
                ((text/event-stream
                  . ,(plz-event-source:text/event-stream
                      :events `((open . ,(lambda (event)
                                           (opencode--log-event "OPEN" event)))
                                (message . opencode--handle-global-event)
                                (close . opencode-disconnect))))))
          :headers (delq nil (list (opencode--auth-header)))
          :then 'opencode-disconnect
          :else 'opencode-disconnect))
  (set-process-query-on-exit-flag opencode--event-subscription nil))

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
