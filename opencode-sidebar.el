;;; opencode-sidebar.el --- Sidebar for OpenCode sessions -*- lexical-binding: t; -*-

;;; Commentary:

;; A lightweight sidebar showing the current project's sessions.  It is
;; intentionally implemented with tabulated-list-mode so it stays fast and
;; works with the normal Emacs side-window machinery.

;;; Code:

(require 'tabulated-list)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-sessions)
(require 'url-util)

(defvar opencode-sidebar-buffer-name "*OpenCode Sessions*")

(defvar opencode-sidebar-mode-map
  (define-keymap
    "g" #'opencode-sidebar-refresh
    "RET" #'opencode-sidebar-open
    "o" #'opencode-sidebar-open
    "q" #'opencode-sidebar-hide))

(define-derived-mode opencode-sidebar-mode tabulated-list-mode "OpenCode Sidebar"
  "Major mode for the OpenCode session sidebar."
  (setq tabulated-list-format
        [("Session" 28 t)
         ("Agent" 10 t)
         ("Status" 8 t)
         ("Updated" 10 t)]
        tabulated-list-sort-key '("Updated" . t)
        tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun opencode-sidebar--directory ()
  "Return the directory the sidebar should show."
  (cond
   ((and (boundp 'opencode-session-id) opencode-session-id)
    default-directory)
   ((project-current)
    (project-root (project-current)))
   (t default-directory)))

(defun opencode-sidebar-visible-p ()
  "Return non-nil when the sidebar buffer is visible."
  (and (get-buffer opencode-sidebar-buffer-name)
       (get-buffer-window opencode-sidebar-buffer-name)))

(defun opencode-sidebar-refresh ()
  "Refresh the sidebar for the current project."
  (interactive)
  (let ((directory (directory-file-name
                    (expand-file-name (opencode-sidebar--directory)))))
    (with-current-buffer (get-buffer-create opencode-sidebar-buffer-name)
      (unless (derived-mode-p 'opencode-sidebar-mode)
        (opencode-sidebar-mode))
      (setq default-directory (file-name-as-directory directory))
      (opencode-api-sessions
          ((url-hexify-string directory))
          sessions
        (when (buffer-live-p (get-buffer opencode-sidebar-buffer-name))
          (with-current-buffer opencode-sidebar-buffer-name
            (setq tabulated-list-entries
                  (mapcar
                   (lambda (session)
                     (let-alist session
                       (list (alist-get 'id session)
                             (vector (or .title "-")
                                     (or .agent "-")
                                     (let ((buffer
                                            (seq-find
                                             (lambda (buf)
                                               (with-current-buffer buf
                                                 (and (derived-mode-p 'opencode-session-mode)
                                                      (equal opencode-session-id
                                                             (alist-get 'id session)))))
                                             (buffer-list))))
                                       (if buffer
                                           (with-current-buffer buffer
                                             (if (string= opencode-session-status "busy")
                                                 "busy" "idle"))
                                         "closed"))
                                     (opencode--format-time-ago
                                      (opencode--time-ago session 'updated))))))
                   sessions))
            (tabulated-list-print t)))))))

(defun opencode-sidebar--session-at-point ()
  "Return the session id at point."
  (tabulated-list-get-id))

(defun opencode-sidebar-open ()
  "Open the session at point."
  (interactive)
  (let ((session-id (opencode-sidebar--session-at-point)))
    (unless session-id
      (user-error "No session at point"))
    (opencode-api-session (session-id)
        session
      (opencode-open-session session))))

(defun opencode-sidebar-hide ()
  "Hide the sidebar window."
  (interactive)
  (when-let ((window (get-buffer-window opencode-sidebar-buffer-name)))
    (delete-window window)))

(defun opencode-sidebar-toggle ()
  "Show or hide the OpenCode session sidebar."
  (interactive)
  (if (opencode-sidebar-visible-p)
      (opencode-sidebar-hide)
    (let ((buffer (get-buffer-create opencode-sidebar-buffer-name)))
      (with-current-buffer buffer
        (unless (derived-mode-p 'opencode-sidebar-mode)
          (opencode-sidebar-mode))
        (setq default-directory
              (file-name-as-directory
               (expand-file-name (opencode-sidebar--directory)))))
      (display-buffer-in-side-window
       buffer
       '((side . right)
         (slot . 0)
         (window-width . 42)
         (preserve-size . (t . t))))
      (select-window (get-buffer-window buffer))
      (opencode-sidebar-refresh))))

(provide 'opencode-sidebar)
;;; opencode-sidebar.el ends here
