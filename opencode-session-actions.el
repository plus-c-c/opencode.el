;;; opencode-session-actions.el --- Session lifecycle actions -*- lexical-binding: t; -*-

;;; Commentary:

;; Opening, replaying, renaming, aborting and compacting sessions.

;;; Code:

(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-render)
(require 'opencode-model)
(require 'opencode-prompt)

(defvar opencode-session-id)
(defvar opencode-session-tokens)
(defvar opencode-session-status)
(defvar opencode-session-agent)
(defvar opencode-session-agents)
(defvar opencode-session-buffers)
(defvar opencode-last-session-buffer)
(defvar opencode--tool-calls-displayed)
(defvar opencode-assistant-messages)
(defvar opencode--table-scan-marker)
(defvar opencode--skip-files-finished-editing-hook-on-next-idle)
(declare-function opencode--with-session-buffer "opencode-sessions" (session-id &rest body))

(defun opencode-kill-session (&optional session)
  "Kill SESSION."
  (interactive)
  (opencode-api-delete-session ((or (alist-get 'id session)
                                    opencode-session-id))
      _result
    ;; if called from session control buffer, don't kill buffer
    ;; but if called from a session buffer then kill this buffer
    (unless session
      (kill-buffer (current-buffer)))))


(defun opencode-session--message-updated (info)
  "Handle message.updated event with INFO."
  (let-alist info
    (pcase .role
      ("assistant"
       (when .time.completed
         (opencode--with-session-buffer .sessionID
           (opencode--align-finished-tables t)
           (setq opencode-session-tokens
                 (opencode--token-total .tokens))
           (force-mode-line-update)))
       (if (or .finish .error)
           (setf opencode-assistant-messages
                 (assoc-delete-all .id opencode-assistant-messages))
         (push (cons .id nil) opencode-assistant-messages))))))


(defun opencode-session--update-part (part-id part delta type)
  "Display PART, partial message output. DELTA is new text since last update.
TYPE is text|reasoning|tool|step-finish
PART-ID is id of PART"
  (let-alist part
    (if .time.end
        (remhash part-id opencode-part-type)
      ;; only will follow up with message.part.delta updates when it has "" as text
      (when (string-empty-p .text)
        (puthash part-id type opencode-part-type)))
    (opencode--with-session-buffer .sessionID
      (when-let* ((message-parts (assoc-string .messageID opencode-assistant-messages)))
        (let* ((last-type (cadr message-parts))
               (last-part-id (cddr message-parts))
               (current-type (pcase type
                               ("reasoning" 'reasoning)
                               ("text" 'text)
                               ("tool" 'tool)
                               (_ nil)))
               (new-block-p (not (equal part-id last-part-id))))
          ;; don't display margins on extra whitespace
          (when (and delta
                     current-type
                     new-block-p)
            (setf delta (string-trim-left delta)))
          (when (and current-type new-block-p)
            (opencode--align-finished-tables t)
            (when (and (eq current-type 'text)
                       (not (eq last-type 'text)))
              (setf opencode-last-response-start
                    (opencode--session-process-position)))
            (when last-type
              (opencode--maybe-insert-block-spacing))
            (when (eq current-type 'text)
              (setq opencode--table-scan-marker
                    (copy-marker (opencode--session-process-position))))
            (setf (cdr message-parts) (cons current-type part-id)))
          (pcase type
            ((and "reasoning" (guard delta))
             (opencode--insert-reasoning-block delta))
            ((and "text" (guard delta))
             (opencode--output delta))
            ("tool"
             (when (and
                    ;; only when it first starts running
                    (string= .state.status "running")
                    ;; avoid duplicate display
                    (not (gethash .callID opencode--tool-calls-displayed))
                    ;; skip the question tool, handled by question.asked event
                    (not (string= .tool "question")))
               (puthash .callID t opencode--tool-calls-displayed)
               (opencode--insert-tool-block .tool .state.input))
             (opencode--maybe-insert-tool-output part))
            ("step-finish"
             (when (string= "stop" .reason)
               (opencode--maybe-insert-block-spacing))))
          (when (and (equal type "text")
                     (or .time.end (and delta (string-search "\n" delta))))
            (opencode--align-finished-tables .time.end)))))))


(defun opencode--replay-user-request (message)
  "Replay a user request MESSAGE."
  (opencode--show-prompt)
  (let-alist message
    (dolist (part .parts)
      (let-alist part
        (unless .synthetic
          (pcase .type
            ("text" (insert .text))))))
    (insert "\n")
    (let ((comint-input-sender #'opencode--highlight-input))
      (comint-send-input))
    (when .info.agent
      (when-let* ((agent (seq-find (lambda (agent)
                                    (equal (or (alist-get 'id agent)
                                               (alist-get 'name agent))
                                           .info.agent))
                                  opencode-session-agents)))
        (setq opencode-session-agent agent)))
    (when .info.model
      (setq opencode-session-agent (or opencode-session-agent
                                       (car opencode-session-agents)))
      (setf (alist-get 'model opencode-session-agent) .info.model)
      (setf (alist-get 'variant opencode-session-agent)
            (alist-get 'variant .info.model)))))


(defun opencode-open-session-same-window (session)
  "Open SESSION using the current window."
  (opencode-open-session session :pop-to-buffer nil))


(defun opencode--replay-session-messages (messages)
  "Display past session MESSAGES."
  (let ((inhibit-redisplay t)
        (comint-output-filter-functions
         (remove #'comint-postoutput-scroll-to-bottom
                 comint-output-filter-functions)))
    (dolist (message messages)
      (let-alist (alist-get 'info message)
        (pcase .role
          ("user" (opencode--replay-user-request message))
          ("assistant"
           (let (last-part-type)
             (dolist (part (alist-get 'parts message))
               (let-alist part
                 (if (string= .type "tool")
                     (progn
                       (opencode--insert-tool-block .tool .state.input)
                       (opencode--maybe-insert-tool-output message))
                   (when .text
                     (let ((text (string-trim .text)))
                       (unless (string-empty-p text)
                         (pcase .type
                           ("text"
                             (unless (equal last-part-type "text")
                               (setf opencode-last-response-start
                                     (opencode--session-process-position)))
                             (setq opencode--table-scan-marker
                                   (copy-marker (opencode--session-process-position)))
                             (opencode--output text)
                             (opencode--align-finished-tables t))
                           ("reasoning"
                            (opencode--insert-reasoning-block
                             text)))
                         (setq last-part-type .type)
                         (opencode--output "\n\n"))))))))
           (setq opencode-session-tokens
                 (opencode--token-total .tokens))))))))


(cl-defun opencode-open-session (session &key (pop-to-buffer t) callback)
  "Open comint based shell for SESSION.
POP-TO-BUFFER controls whether to pop to or switch to the session buffer.
Returns the buffer.
If CALLBACK is given, it will be called with the session after it is initialized."
  (let-alist session
    (let ((old-buffer (gethash .id opencode-session-buffers)))
      (if (buffer-live-p old-buffer)
          (if pop-to-buffer
              (pop-to-buffer old-buffer)
            (switch-to-buffer old-buffer))
        (let* ((buffer (generate-new-buffer
                        (format "*OpenCode: %s*" (or .title "session"))))
               (agents (mapcar (lambda (agent)
                                 (unless (alist-get 'model agent)
                                   (setf (alist-get 'model agent)
                                         opencode-last-model))
                                 agent)
                               (or (copy-tree opencode-agents)
                                   (copy-tree opencode-session-agents))))
               (selected (or (seq-find
                              (lambda (agent)
                                (member (alist-get 'agent session)
                                        (list (alist-get 'id agent)
                                              (alist-get 'name agent))))
                              agents)
                             (car agents))))
          (with-current-buffer buffer
            (opencode-session-mode)
            (setq opencode-session-id .id
                  opencode-last-session-buffer buffer
                  default-directory (file-name-as-directory
                                     (or .directory default-directory))
                  opencode--tool-calls-displayed (make-hash-table :test 'equal)
                  opencode-session-agents agents
                  opencode-session-agent selected
                  opencode-session-tokens (opencode--token-total .tokens)
                  mode-line-process '(:eval (opencode--session-status-indicator)))
            (when (and selected .model)
              (setf (alist-get 'model selected) .model)
              (setf (alist-get 'variant selected)
                    (alist-get 'variant .model)))
            (hack-dir-local-variables-non-file-buffer)
            (add-hook 'fill-nobreak-predicate #'opencode--in-label-p nil t)
            (puthash .id buffer opencode-session-buffers)
            (let ((proc (start-process "dummy" buffer nil)))
              (set-process-query-on-exit-flag proc nil)
              (add-hook 'kill-buffer-hook #'opencode--session-cleanup nil t)
              (opencode-insert-logo)
              (opencode-api-session-messages (.id)
                  messages
                (opencode--replay-session-messages messages)
                (opencode-api-sessions-status
                  busy-sessions
                  (if-let* ((session-status (cdr (assoc-string .id busy-sessions))))
                      (opencode-session--set-status
                       .id
                       (alist-get 'type session-status))
                    (opencode--show-prompt))
                  (when callback
                    (funcall callback session)))))
            (if pop-to-buffer
                (pop-to-buffer buffer)
              (switch-to-buffer buffer))))))))


(defun opencode--current-message-number ()
  "Return the 0-indexed message number at point.
Counts prompts from the beginning of the buffer to the current position.
Returns nil if point is before the first prompt."
  (save-excursion
    (end-of-line)
    (comint-previous-prompt 1)
    (let ((target-point (point)))
      (goto-char (point-min))
      (cl-loop do (comint-next-prompt 1)
               while (< (point) target-point)
               count t))))


(defmacro opencode--current-message-id (result &rest body)
  "Run BODY with RESULT as the message id of the user message at point."
  (declare (indent defun))
  `(opencode--current-message-exchange (,result _assistant-id)
     ,@body))


(defmacro opencode--current-message-exchange (bindings &rest body)
  "Run BODY with BINDINGS (user-id assistant-id)
bound to the exchange ids at point."
  (declare (indent defun))
  (let ((user-id (car bindings))
        (assistant-id (cadr bindings)))
    `(when-let* ((message-number (opencode--current-message-number)))
       (opencode-api-session-messages (opencode-session-id)
           messages
         (let* ((user-message-index
                 (cl-loop for message in messages
                          for index from 0
                          when (string= "user" (map-nested-elt message '(info role)))
                          count t into count
                          when (= (1- count) message-number)
                          return index))
                (user-message (and user-message-index
                                   (nth user-message-index messages)))
                (assistant-message (and user-message-index
                                        (let ((next-message (nth (1+ user-message-index)
                                                                 messages)))
                                          (when (string= "assistant"
                                                         (map-nested-elt next-message
                                                                         '(info role)))
                                            next-message))))
                (,user-id (map-nested-elt user-message '(info id)))
                (,assistant-id (map-nested-elt assistant-message '(info id))))
           ,@body)))))


(defun opencode--delete-message-at-point ()
  "Delete the prompt at point and its output from the session buffer."
  (let ((start (save-excursion
                 (end-of-line)
                 (comint-previous-prompt 1)
                 (line-beginning-position)))
        (end (save-excursion
               (end-of-line)
               (comint-previous-prompt 1)
               (goto-char (line-beginning-position 2))
               (if (ignore-errors (comint-next-prompt 1) t)
                   (line-beginning-position)
                 (point-max)))))
    (let ((inhibit-read-only t))
      (remove-overlays start end)
      (delete-region start end))))


(defun opencode-rename-session (&optional session)
  "Rename SESSION. If in a session buffer, rename that session."
  (interactive)
  (let* ((old-title (or (alist-get 'title session)
                        (string-remove-prefix
                         "*OpenCode: "
                         (string-trim-right (buffer-name)
                                            "\\*\\(<[0-9]+>\\)?"))))
         (title (read-string "Title: " old-title)))
    (opencode-api-rename-session ((or (alist-get 'id session)
                                      opencode-session-id))
        `((title . ,title))
        _res)))


(defun opencode-session--display-error (session-id message)
  "Display error MESSAGE in SESSION-ID and then new prompt."
  (opencode--with-session-buffer session-id
    (opencode--output (propertize message 'face 'error))
    (opencode--output "\n\n")
    (opencode--show-prompt)))


(defun opencode-abort-session ()
  "Abort a busy session and go back to prompt."
  (interactive)
  (setq opencode--skip-files-finished-editing-hook-on-next-idle t)
  (opencode-api-abort-session (opencode-session-id)
      interrupted
    (if interrupted
        (message "Aborted session")
      (setq opencode--skip-files-finished-editing-hook-on-next-idle nil)
      (message "Session is not running"))))


(defun opencode-compact-session ()
  "Compact the current session."
  (interactive)
  (if opencode-session-id
      (opencode-api-summarize (opencode-session-id)
          '((delivery . "steer"))
          _result
        (message "Compaction requested"))
    (user-error "Not in an OpenCode session")))


(defun opencode-scroll-to-last-response-start ()
  "Scroll to the top of the last text part of assistant response."
  (interactive)
  (unless opencode-last-response-start
    (user-error "No last response"))
  (goto-char opencode-last-response-start)
  (forward-word 1)
  (backward-word 1)
  (recenter-top-bottom 0))


(provide 'opencode-session-actions)
;;; opencode-session-actions.el ends here
