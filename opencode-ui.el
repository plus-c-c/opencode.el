;;; opencode-ui.el --- Window lifecycle commands for OpenCode -*- lexical-binding: t; -*-

;;; Commentary:

;; OpenCode buffers stay alive in the background.  These commands show the
;; last session full-frame and restore the previous window layout, in the
;; spirit of ebib's show/hide workflow.

;;; Code:

(require 'opencode-common)
(require 'opencode-sessions)
(require 'opencode-sidebar)

(defcustom opencode-show-fullscreen t
  "Whether `opencode-show' should display the session full-frame."
  :type 'boolean
  :group 'opencode)

(defvar opencode--saved-window-config nil
  "Window configuration saved before `opencode-show'.")

(defun opencode--display-session-buffer ()
  "Return a live OpenCode session buffer for display."
  (or (and (derived-mode-p 'opencode-session-mode) (current-buffer))
      (and (buffer-live-p opencode-last-session-buffer)
           opencode-last-session-buffer)
      (seq-find (lambda (buffer)
                  (with-current-buffer buffer
                    (derived-mode-p 'opencode-session-mode)))
                (buffer-list))))

(defcustom opencode-show-header-line t
  "Whether to show a compact OpenCode header line in session buffers."
  :type 'boolean
  :group 'opencode)

(defun opencode-ui--title ()
  "Return a short title for the current session buffer."
  (let ((name (buffer-name)))
    (if (string-match "^\\*OpenCode: \\(.+?\\)\\*$" name)
        (match-string 1 name)
      name)))

(defun opencode-ui--header-line ()
  "Return the OpenCode session header line string."
  (let* ((agent (or (alist-get 'name opencode-session-agent) "?"))
         (model (ignore-errors (opencode--current-model)))
         (model-name (or (and model (alist-get 'name model)) ""))
         (limit (and model (map-nested-elt model '(limit context))))
         (context (if (and limit (> limit 0))
                      (format "%.0f%%" (* 100 (/ (float opencode-session-tokens)
                                                 limit)))
                    "")))
    (string-join (delq nil
                       (list (opencode-ui--title)
                             (format "[%s]" agent)
                             model-name
                             context))
                 "  ")))

(defun opencode-ui--setup-header-line ()
  "Install the header line in the current session buffer."
  (when opencode-show-header-line
    (setq-local header-line-format
                '(:eval (opencode-ui--header-line)))))

(defun opencode-tab-line-tabs ()
  "Return open OpenCode session buffers for tab-line."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer
                  (derived-mode-p 'opencode-session-mode)))
              (buffer-list)))

(defun opencode-tab-line-mode ()
  "Show OpenCode sessions in the tab line."
  (interactive)
  (setq-local tab-line-tabs-function #'opencode-tab-line-tabs)
  (tab-line-mode 1))

(defun opencode-show ()
  "Show the last OpenCode session in the foreground.
The session process keeps running while hidden."
  (interactive)
  (let ((buffer (opencode--display-session-buffer)))
    (unless buffer
      (user-error "No OpenCode session buffer; run `M-x opencode' first"))
    (setq opencode--saved-window-config (current-window-configuration))
    (pop-to-buffer buffer)
    (when opencode-show-fullscreen
      (delete-other-windows))
    (setq opencode-last-session-buffer buffer)))

(defun opencode-hide ()
  "Hide the OpenCode session and restore the previous window layout."
  (interactive)
  (if opencode--saved-window-config
      (progn
        (set-window-configuration opencode--saved-window-config)
        (setq opencode--saved-window-config nil))
    (when (derived-mode-p 'opencode-session-mode)
      (bury-buffer))))

(defun opencode-toggle ()
  "Toggle between the OpenCode session and the previous layout."
  (interactive)
  (if (derived-mode-p 'opencode-session-mode)
      (opencode-hide)
    (opencode-show)))

(add-hook 'opencode-session-mode-hook #'opencode-ui--setup-header-line)

(with-eval-after-load 'opencode-sessions
  (define-key opencode-session-mode-map (kbd "C-c C-q") #'opencode-hide)
  (define-key opencode-session-mode-map (kbd "C-c C-o") #'opencode-sidebar-toggle)
  (define-key opencode-session-mode-map (kbd "C-c C-d") #'opencode-send-diagnostics))

(provide 'opencode-ui)
;;; opencode-ui.el ends here
