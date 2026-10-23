;;; opencode-flymake.el --- Flymake/Eglot diagnostics for OpenCode -*- lexical-binding: t; -*-

;;; Commentary:

;; V2 does not run LSP servers.  When Emacs uses Eglot (Flymake) or another
;; Flymake backend, report its diagnostics to the model after file edits.

;;; Code:

(declare-function flymake-mode "flymake" (&optional arg))
(declare-function flymake-start "flymake" (&optional force))
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-text "flymake" (diag))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function opencode-hook-finished "opencode" ())
(declare-function opencode-report-diagnostic "opencode" (message))

(defvar flymake-mode)
(defvar flycheck-mode)

(defun opencode--flymake-report (file diagnostics)
  "Return a formatted report for DIAGNOSTICS in FILE."
  (let ((base default-directory)
        (lines
         (cl-loop for diag in diagnostics
                  for type = (flymake-diagnostic-type diag)
                  when (memq type '(:error :warning))
                  collect (format "- %s:%d: %s: %s"
                                  (file-relative-name file base)
                                  (line-number-at-pos
                                   (flymake-diagnostic-beg diag))
                                  (symbol-name type)
                                  (flymake-diagnostic-text diag)))))
    (when lines (mapconcat #'identity lines "\n"))))

(defun opencode--flymake-edited-files (files)
  "Run Flymake on FILES and report diagnostics for the active hook."
  (if (not (and files (require 'flymake nil t)))
      nil
    (let ((base default-directory)
          (remaining (length files)))
      (cl-labels ((finish (report)
                    (when report (opencode-report-diagnostic report))
                    (cl-decf remaining)
                    (when (zerop remaining) (opencode-hook-finished))))
        (dolist (file files)
          (with-current-buffer (find-file-noselect file)
            (revert-buffer t t t)
            (if (bound-and-true-p flycheck-mode)
                (finish nil)
              (unless flymake-mode (flymake-mode 1))
              (flymake-start)
              (let ((buffer (current-buffer)))
                (run-at-time
                 1.0 nil
                 (lambda ()
                   (if (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (finish
                          (opencode--flymake-report
                           file (flymake-diagnostics))))
                     (finish nil)))))))))
      :opencode-async)))
(provide 'opencode-flymake)
;;; opencode-flymake.el ends here
