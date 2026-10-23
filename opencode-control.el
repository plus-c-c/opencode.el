;;; opencode-control.el --- Session control buffer -*- lexical-binding: t; -*-

;;; Commentary:

;; The vtable used to list sessions for a project.

;;; Code:

(require 'opencode-api)
(require 'opencode-common)
(require 'magit)
(require 'vtable)
(require 'url-util)

(defvar opencode-session-control-mode-map
  (define-keymap
    "r" 'opencode-sessions-redisplay
    "g" nil
    "SPC" nil
    "n" 'opencode-new-session
    "M" 'opencode-toggle-mcp
    "U" 'opencode-unshare-all-sessions
    "v" 'opencode-session-control-toggle-verbose))


(defvar-local opencode-session-control-verbose nil
  "Toggle whether to display subagents in session control buffer.")


(defvar-local opencode-session-control-redisplaying nil
  "Non-nil while a session control refresh is in flight.")



(define-derived-mode opencode-session-control-mode special-mode "Sessions"
  "Opencode session control panel mode.")


(defun opencode--tab-dispatch ()
  "Cycle session agent, or move to next button if point is on one."
  (interactive)
  (if (button-at (point))
      (forward-button 1)
    (call-interactively #'opencode-cycle-session-agent)))


(defun opencode-session-control-toggle-verbose ()
  "Toggle verbose mode in session control buffer."
  (interactive)
  (setq opencode-session-control-verbose
        (not opencode-session-control-verbose))
  (opencode-sessions-redisplay))


(defun opencode-sessions-redisplay ()
  "Refresh the session display table for DIRECTORY."
  (interactive)
  (unless opencode-session-control-redisplaying
    (setq opencode-session-control-redisplaying t)
    (opencode-api-sessions
        ((url-hexify-string
          (directory-file-name (expand-file-name default-directory))))
        sessions
      (setq opencode-session-control-redisplaying nil)
      (let ((inhibit-read-only t)
          (inhibit-redisplay t)
          (point (point))
          (sessions (if opencode-session-control-verbose
                        sessions
                      (seq-remove (lambda (session)
                                    (alist-get 'parentID session))
                                  sessions)))
          (cache (make-hash-table :test #'equal)))
      (erase-buffer)
      (if sessions
          (make-vtable
           :columns '("Title"
                      (:name "Branch" :min-width 6)
                      (:name "Last Updated" :width 12
                       :formatter opencode--format-time-ago
                       :primary ascend)
                      (:name "Agent" :width 10)
                      (:name "Created at" :width 10
                       :formatter opencode--format-time-ago))
           :objects sessions
           :actions '("x" opencode-kill-session
                      "R" opencode-rename-session
                      "RET" opencode-open-session-same-window
                      "o" opencode-open-session-same-window)
           :getter (lambda (object column vtable)
                     (let-alist object
                       (pcase (vtable-column vtable column)
                         ("Title" (or .title "-"))
                         ("Branch" (if (and .directory (file-exists-p .directory))
                                       (let ((default-directory .directory))
                                         (or (gethash .directory cache)
                                             (puthash .directory
                                                      (magit-get-current-branch)
                                                      cache)))
                                     "-"))
                         ("Last Updated" (opencode--time-ago object 'updated))
                         ("Agent" (or .agent "-"))
                         ("Created at" (opencode--time-ago object 'created)))))
           :separator-width 3
           :keymap opencode-session-control-mode-map)
        (insert "No sessions in " (or default-directory "unknown directory")))
      (goto-char point)))))


(provide 'opencode-control)
;;; opencode-control.el ends here
