;;; opencode-session-extra.el --- Session stats and transfer -*- lexical-binding: t; -*-

;;; Commentary:

;; V2 session statistics, export and import helpers.

;;; Code:

(require 'json)
(require 'pp)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-sessions)

(defun opencode-session-stats ()
  "Show OpenCode V2 session statistics in a buffer."
  (interactive)
  (opencode-api-stats stats
    (with-current-buffer (get-buffer-create "*OpenCode Stats*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (insert (pp-to-string stats))
        (goto-char (point-min)))
      (display-buffer (current-buffer)))))

(defun opencode-export-session ()
  "Export the current session to a JSON file."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an OpenCode session"))
  (let ((file (read-file-name "Export session to: " nil nil nil
                              (format "opencode-%s.json" opencode-session-id))))
    (opencode-api-export-session (opencode-session-id)
        data
      (with-temp-file file
        (insert (let ((json-encoding-pretty-print t))
                  (json-encode data))))
      (message "Exported session to %s" file))))

(defun opencode-import-session ()
  "Import a session from a JSON file."
  (interactive)
  (let* ((file (read-file-name "Import session from: "))
         (data (with-temp-buffer
                 (insert-file-contents file)
                 (goto-char (point-min))
                 (json-parse-buffer :object-type 'alist))))
    (opencode-api-import-session data
        session
      (opencode-open-session session))))

(provide 'opencode-session-extra)
;;; opencode-session-extra.el ends here
