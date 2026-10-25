;;; opencode-sessions.el --- Code for managing opencode sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Code for managing opencode sessions

;;; Code:

(require 'comint)
(require 'magit)
(require 'mailcap)
(require 'markdown-ts-mode)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-convert)
(require 'opencode-format-tool-calls)
(require 'project)
(require 'url-util)
(require 'vtable)
(require 'yank-media)

(defvar opencode-session-mode-map
  (define-keymap
    "C-<return>" 'opencode-insert-newline
    "C-c C-y" 'opencode-yank-code-block
    "C-c Y" 'opencode-copy-conversation
    "C-c C-c" 'opencode-abort-session
    "C-c C-p" 'opencode-respond-permission
    "C-c x" 'opencode-kill-session
    "<backtab>" 'backward-button
    "TAB" 'opencode--tab-dispatch
    "C-c r" 'opencode-rename-session
    "C-c n" 'opencode-new-session
    "C-c l" 'opencode-select-session
    "C-c c" 'opencode-select-child-session
    "C-c C" 'opencode-compact-session
    "C-c t" 'opencode-scroll-to-last-response-start
    "C-c p" 'opencode-open-parent
    "C-c f" 'opencode-add-file
    "C-c b" 'opencode-add-buffer-dwim
    "C-c s" 'opencode-share-session
    "C-c u" 'opencode-unshare-session
    "C-c U" 'opencode-unshare-all-sessions
    "C-c m" 'opencode-select-model
    "C-c v" 'opencode-select-variant
    "C-c M" 'opencode-toggle-mcp
    "C-c F" 'opencode-fork-session
    "C-c D" 'opencode-delete-message
    "C-c R" 'opencode-revert-message
    "/" 'opencode-insert-slash-command
    "@" 'opencode-add-subagent))

(with-eval-after-load 'evil
  (declare-function evil-define-key* "evil-core")
  (evil-define-key* 'normal opencode-session-control-mode-map
    "r" 'opencode-sessions-redisplay
    "n" 'opencode-new-session
    "gv" 'opencode-session-control-toggle-verbose)
  (evil-define-key* 'insert opencode-session-mode-map
    "@" 'opencode-add-subagent
    "/" 'opencode-insert-slash-command))

(defvar-local opencode-session-id nil
  "Session id for the current opencode session buffer.")

(defvar-local opencode-session-tokens 0
  "Tokens consumed by the current session.")

(defvar-local opencode-session-status "idle"
  "Status of the current opencode session (busy or idle).")

(defvar-local opencode--table-scan-marker nil
  "Marker at the unprocessed Markdown in the current assistant text part.")

(defvar-local opencode-session-agent nil
  "Currently active agent for this buffer's session.")

(defvar-local opencode-session-agents nil
  "List of agents for the current session.
Buffer local so we can configure models and variants per agent per session.")

(defvar-local opencode-session-pending-questions nil
  "Pending questions for this session buffer, awaiting user response.
When non-nil, contains a cons cell (QUESTION-ID . QUESTIONS-VECTOR).")

(defvar-local opencode-session-pending-permission nil
  "Pending permission requests for this session buffer, as a list of plists.
Each plist has keys :id, :session-id, :type, :title, and :marker.
Requests are processed FIFO.")

(defvar-local opencode--temp-files nil
  "Temporary files to delete after sending or killing a session buffer.")

(defvar-local opencode--files-edited-this-turn nil
  "Files edited by opencode during the current turn.")

(defvar-local opencode--skip-files-finished-editing-hook-on-next-idle nil
  "Non-nil means the next session idle should not run finished-editing hooks.")

(defvar opencode-session-buffers
  (make-hash-table :test 'equal)
  "A mapping of session ids to Emacs buffers.")

(defvar-local opencode--tool-calls-displayed nil
  "A hash table containing all callID that have already been displayed.")

(defvar-local opencode-last-response-start nil
  "Position of the start of the latest assistant text block.")

(defvar opencode-assistant-messages
  nil
  "An alist mapping updating assistant message ids to (type . part-id).")

(defmacro opencode--with-session-buffer (session-id &rest body)
  "Run BODY in the live session buffer for SESSION-ID."
  (declare (indent 1))
  `(when-let* ((buffer (gethash ,session-id opencode-session-buffers)))
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         ,@body))))

(defun opencode--session-buffer-p (buf)
  "Return non-nil if BUF is a opencode session buffer.
Accepts (\"name\" . buffer) to work as a read-buffer predicate."
  (with-current-buffer (cl-etypecase buf
                         ((or string buffer) buf)
                         (list (cdr buf)))
    opencode-session-id))

(defun opencode-select-open-session ()
  "Select among open session buffers."
  (interactive)
  (switch-to-buffer
   (read-buffer "Switch to: " nil t 'opencode--session-buffer-p)))

(defun opencode-consult-sessions ()
  "Run `consult-line-multi' across all open opencode sessions."
  (interactive)
  (if (require 'consult nil t)
      (progn
        (declare-function consult-line-multi "consult")
        (consult-line-multi '(:predicate opencode--session-buffer-p)))
    (user-error "This requires consult")))

(defun opencode-visit-last-idle ()
  "Open the most recent session to notify as idle."
  (interactive)
  (if opencode-alerted-sessions
      (opencode-open-session (pop opencode-alerted-sessions))
    (message "No idle sessions")))

(declare-function opencode-autoconnect "opencode")

(defun opencode-select-session ()
  "Select and open a session from the current project using completion."
  (interactive)
  (opencode-autoconnect
   (lambda ()
     (opencode-api-sessions ((url-hexify-string (directory-file-name (expand-file-name default-directory))))
         sessions
       (opencode--select-sessions "Session: "
                                  (seq-remove (lambda (session)
                                                (alist-get 'parentID session))
                                              sessions)
                                  (format "No sessions in %s" default-directory)
                                  :pop-to-buffer nil)))))

(cl-defun opencode--select-sessions (prompt sessions no-sessions-message &key (pop-to-buffer t))
  "Select a session from SESSIONS to open.
Use PROMPT and display NO-SESSIONS-MESSAGE if SESSIONS is empty.
POP-TO-BUFFER controls whether to pop to or switch to the session buffer."
  (if sessions
      (opencode-open-session
       (opencode--annotated-completion
        prompt
        (cl-loop for session in sessions
                 collect (let-alist session
                           (list .title
                                 session
                                 (opencode--format-time-ago
                                  (opencode--time-ago
                                   session 'updated))
                                 (opencode--time-ago
                                  session 'updated)))))
       :pop-to-buffer pop-to-buffer)
    (message no-sessions-message)))

(defun opencode-search-sessions (query)
  "Search all sessions in the current project for QUERY."
  (interactive "sSession search: ")
  (opencode-autoconnect
   (lambda ()
     (opencode-api-session-search
         ((url-hexify-string query)
          (url-hexify-string
           (directory-file-name (expand-file-name default-directory))))
         sessions
       (opencode--select-sessions "Session: " sessions "No matching sessions")))))

(defun opencode-select-idle ()
  "Select a session that hasn't been visited since it went idle."
  (interactive)
  (opencode--select-sessions "Session: " opencode-alerted-sessions "No idle sessions"))

(defun opencode-select-child-session ()
  "Open a child (subagent) session of the current session."
  (interactive)
  (opencode-api-session-children (opencode-session-id)
      children
    (opencode--select-sessions "Subagent: " children "No children")))

(defun opencode-open-parent ()
  "Open the parent of the current session."
  (interactive)
  (opencode-api-session (opencode-session-id)
      session
    (opencode-api-session ((alist-get 'parentID session))
        parent
      (opencode-open-session parent))))

(defun opencode-share-session (&optional _session)
  "OpenCode V2 has no HTTP share endpoint."
  (interactive)
  (user-error "Session sharing is not available in OpenCode V2's HTTP API"))

(defun opencode-unshare-session (&optional _session)
  "OpenCode V2 has no HTTP share endpoint."
  (interactive)
  (user-error "Session sharing is not available in OpenCode V2's HTTP API"))

(defun opencode-unshare-all-sessions ()
  "OpenCode V2 has no HTTP share endpoint."
  (interactive)
  (user-error "Session sharing is not available in OpenCode V2's HTTP API"))

(defun opencode-insert-slash-command ()
  "Insert an opencode slash command."
  (interactive)
  (if (= (point) (cdr comint-last-prompt))
      (let* ((directory (opencode--normalize-directory default-directory))
             (commands (alist-get directory opencode--slash-commands-by-directory
                                  nil nil #'string=))
             (command (opencode--annotated-completion
                       "Slash command: "
                       (cl-loop for command in commands
                                collect (let-alist command
                                          (list
                                           .name
                                           .name
                                           .description))))))
        (when command
          (insert (concat "/" command))))
    (call-interactively #'self-insert-command)))

(defun opencode--session-status-indicator ()
  "Return mode line indicator for session status."
  (let-alist opencode-session-agent
    (let* ((agent (pcase .name
                    ("Planner-Sisyphus" "Planner")
                    (name name)))
           (model (opencode--current-model))
           (status (pcase opencode-session-status
                     ("busy" "⏳")
                     ("idle" "🚀")
                     (_ "")))
           (limit (and model (map-nested-elt model '(limit context))))
           (context-used (if (and limit (> limit 0))
                             (* 100 (/ (float opencode-session-tokens) limit))
                           0)))
      (if (< (window-width) 115)
          (format "[🤖 %s] %.0f%%%% %s  " agent context-used status)
        (format "[🤖 %s - %s] %.0f%%%% %s  "
                agent
                (concat (or (alist-get 'name model) "")
                        (when .variant
                          (propertize (format " %s" .variant)
                                      'face '(bold opencode-request-margin-highlight))))
                context-used status)))))

(defun opencode-session--set-status (session-id status)
  "Set STATUS for the session with SESSION-ID and update modeline."
  (opencode--with-session-buffer session-id
    (setq opencode-session-status status)
    (force-mode-line-update)))

(defun opencode--defer-files-finished-editing-hook (session-id)
  "Return and clear SESSION-ID's next-idle finished-editing hook skip flag."
  (opencode--with-session-buffer session-id
    (prog1 opencode--skip-files-finished-editing-hook-on-next-idle
      (setq opencode--skip-files-finished-editing-hook-on-next-idle nil))))

(define-derived-mode opencode-session-mode comint-mode "OpenCode"
  "Major mode for interacting with an opencode session."
  (setq-local comint-use-prompt-regexp nil
              comint-input-sender 'opencode--send-input
              comint-highlight-input nil
              left-margin-width (1+ left-margin-width))
  (setq-local markdown-ts-hide-markup t
              markdown-ts-enable-code-block-context-mode nil
              markdown-ts-enable-table-mode nil)
  ;; Set up Markdown's parser and font-lock without replacing comint as the
  ;; major mode (and losing its process, prompt, and input handling).
  (unless (and (treesit-language-available-p 'markdown)
               (treesit-language-available-p 'markdown-inline))
    (error "OpenCode sessions require the markdown and markdown-inline Tree-sitter grammars"))
  (markdown-ts-mode--initialize)
  ;; Markdown's conventional-mode helper uses an indirect buffer sharing all
  ;; session properties.  Mode initialization/cleanup there can interfere with
  ;; already-fontified Markdown.  Collect code faces in an isolated buffer.
  (setq-local font-lock-defaults
              '(nil nil nil nil
                    (font-lock-fontify-syntactically-function
                     . opencode--fontify-markdown)))
  (setq-local font-lock-fontify-syntactically-function #'opencode--fontify-markdown)
  ;; File-reference buttons are created by comint output, not font-lock.
  (setq-local font-lock-extra-managed-props
              (cl-set-difference font-lock-extra-managed-props
                                 '(button category action help-echo)))
  (visual-line-mode)
  (cursor-intangible-mode)
  (yank-media-handler "\\`image/" #'opencode--yank-image))

(defun opencode-yank-code-block ()
  "Yank the code inside the markdown code block under point."
  (interactive)
  (let* ((node (treesit-parent-until (treesit-node-at (point) 'markdown)
                                     "fenced_code_block" t))
         (content (and node (treesit-search-subtree node "code_fence_content"))))
    (unless node
      (user-error "Not in a fenced code block"))
    (if content
        (copy-region-as-kill (treesit-node-start content) (treesit-node-end content))
      (kill-new ""))))

(defun opencode-copy-conversation ()
  "Copy the current conversation as a Markdown transcript.
The transcript is built from raw session messages, so hidden tool output is
included regardless of `opencode-show-tool-output'.

Useful if you have separate usage limits for opencode vs webchat, so you can let
it gather context and start a plan in opencode, and then iterate on the plan
in webchat."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an OpenCode session"))
  (message "Fetching conversation...")
  (opencode-api-session-messages (opencode-session-id)
      messages
    (let ((transcript (opencode--conversation-to-markdown messages)))
      (kill-new transcript)
      (message "Copied conversation (%d messages, %d characters)"
               (length messages)
               (length transcript)))))

(require 'opencode-render)
(require 'opencode-model)
(require 'opencode-prompt)
(require 'opencode-control)
(require 'opencode-session-actions)

(provide 'opencode-sessions)
;;; opencode-sessions.el ends here
