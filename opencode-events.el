;;; opencode-events.el --- OpenCode V2 event handling -*- lexical-binding: t; -*-

;;; Commentary:

;; Parses the V2 SSE stream and translates flat events into UI updates.

;;; Code:

(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-diff-parser)
(require 'opencode-v2)
(require 'opencode-sessions)
(require 'opencode-permission)
(require 'opencode-question)
(require 'opencode-form)
(require 'plz-media-type)
(require 'plz-event-source)

(declare-function opencode-disconnect "opencode-server" (&optional event))
(defvar opencode-api-url)
(defvar opencode--event-subscription)

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
               opencode--session-control-buffers))
    (when (and (fboundp 'opencode-sidebar-refresh)
               (fboundp 'opencode-sidebar-visible-p)
               (opencode-sidebar-visible-p))
      (opencode-sidebar-refresh))))


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


(defun opencode--record-v2-tool-files (session-id name input output-paths
                                                 &optional metadata)
  "Record and run hooks for files edited by tool NAME with INPUT.
OUTPUT-PATHS is the list of paths reported by the V2 tool result.
METADATA may contain a V1-style diff for line ranges."
  (let* ((diff (and metadata (alist-get 'diff metadata)))
         (ranges (and diff
                      (ignore-errors
                        (opencode-diff->source-line-ranges diff))))
         (files (or ranges
                    (delq nil (cons (opencode-v2--tool-file name input)
                                    (append output-paths nil))))))
    (when files
      (opencode--with-session-buffer session-id
        (dolist (file files)
          (let* ((raw (if (consp file) (car file) file))
                 (range (if (consp file) (cdr file) nil)))
            (when-let ((path (opencode--resolve-file-reference raw)))
              (unless (member path opencode--files-edited-this-turn)
                (cl-pushnew path opencode--files-edited-this-turn :test #'equal)
                (run-hook-with-args 'opencode-file-edited-functions
                                    path range)))))))))


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
          session-id name input (alist-get 'outputPaths data)
          (alist-get 'metadata data))
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


(defvar opencode-event-log-max-lines nil
  "Maximum number of lines to log in the opencode event log buffer.
Or nil to disable logging.")


(provide 'opencode-events)
;;; opencode-events.el ends here
