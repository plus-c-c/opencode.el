;;; opencode-filehooks.el --- File edit and diagnostic hooks -*- lexical-binding: t; -*-

;;; Commentary:

;; Hooks that run after OpenCode edits files, plus helpers for feeding
;; formatter, Flycheck, pytest and ERT results back to the model.

;;; Code:

(require 'opencode-common)
(require 'opencode-diff-parser)
(require 'opencode-sessions)
(require 'opencode-prompt)
(require 'flycheck nil t)

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


(provide 'opencode-filehooks)
;;; opencode-filehooks.el ends here
