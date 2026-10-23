;;; opencode-diagnostics.el --- Emacs-side diagnostics for OpenCode -*- lexical-binding: t; -*-

;;; Commentary:

;; OpenCode V2 no longer runs language servers.  Keep LSP diagnostics in
;; Emacs (Eglot uses Flymake, lsp-mode can use Flycheck) and send the
;; collected errors to the current session when useful.

;;; Code:

(require 'opencode-common)
(require 'opencode-sessions)
(require 'opencode-prompt)
(require 'opencode-filehooks)

(defun opencode-diagnostics--flycheck (buffer)
  "Return Flycheck errors for BUFFER."
  (with-current-buffer buffer
    (when (and (boundp 'flycheck-mode) flycheck-mode
               (boundp 'flycheck-current-errors))
      (cl-loop for err in flycheck-current-errors
               when (memq (flycheck-error-level err) '(error warning))
               collect (format "%s:%s: %s: %s"
                               (or (flycheck-error-filename err)
                                   (buffer-file-name buffer)
                                   (buffer-name buffer))
                               (or (flycheck-error-line err) 1)
                               (symbol-name (flycheck-error-level err))
                               (flycheck-error-format err nil))))))

(defun opencode-diagnostics--flymake (buffer)
  "Return Flymake diagnostics for BUFFER."
  (with-current-buffer buffer
    (when (and (boundp 'flymake-mode) flymake-mode
               (fboundp 'flymake-diagnostics))
      (cl-loop for diag in (flymake-diagnostics)
               for beg = (flymake-diagnostic-beg diag)
               collect (format "%s:%d: %s: %s"
                               (or (buffer-file-name buffer)
                                   (buffer-name buffer))
                               (line-number-at-pos beg)
                               (symbol-name (flymake-diagnostic-type diag))
                               (flymake-diagnostic-text diag))))))

(defun opencode-diagnostics--collect (buffer)
  "Collect diagnostics for BUFFER from Flycheck or Flymake."
  (or (opencode-diagnostics--flycheck buffer)
      (opencode-diagnostics--flymake buffer)))

(defun opencode-diagnostics-report-buffer (&optional buffer)
  "Report diagnostics for BUFFER through the active editing hook."
  (interactive)
  (let ((buffer (or buffer (current-buffer)))
        (diagnostics (opencode-diagnostics--collect
                      (or buffer (current-buffer)))))
    (unless (bound-and-true-p opencode--files-finished-editing-current)
      (user-error "No active editing hook; use `opencode-send-diagnostics'"))
    (if diagnostics
        (opencode-report-diagnostic
         (concat "Diagnostics from Emacs:\n\n"
                 (mapconcat (lambda (line) (concat "- " line))
                            diagnostics "\n")))
      (message "No Flycheck/Flymake diagnostics in %s"
               (buffer-name buffer)))))

(defun opencode-send-diagnostics (&optional buffer)
  "Send diagnostics for BUFFER to the current OpenCode session."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (diagnostics (opencode-diagnostics--collect buffer))
         (session-buffer (or (and (buffer-live-p opencode-last-session-buffer)
                                  opencode-last-session-buffer)
                             (seq-find
                              (lambda (buf)
                                (with-current-buffer buf
                                  (derived-mode-p 'opencode-session-mode)))
                              (buffer-list)))))
    (unless diagnostics
      (user-error "No Flycheck/Flymake diagnostics in %s" (buffer-name buffer)))
    (unless session-buffer
      (user-error "No OpenCode session buffer"))
    (with-current-buffer session-buffer
      (opencode-session--send-synthetic-input
       (concat "Emacs reports the following diagnostics. Please fix them.\n\n"
               (mapconcat (lambda (line) (concat "- " line))
                          diagnostics "\n"))))))

(provide 'opencode-diagnostics)
;;; opencode-diagnostics.el ends here
