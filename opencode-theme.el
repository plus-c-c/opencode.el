;;; opencode-theme.el --- Dark theme for OpenCode buffers -*- lexical-binding: t; -*-

;;; Commentary:

;; Buffer-local dark palette inspired by Dracula and the OpenCode TUI.
;; The theme deliberately uses muted diff colors instead of pure red/green.

;;; Code:

(require 'face-remap)

(defcustom opencode-use-dark-theme t
  "Whether to apply the OpenCode dark palette in session buffers."
  :type 'boolean
  :group 'opencode)

(defconst opencode-dark--palette
  '((bg . "#1e1f29")
    (surface . "#21222c")
    (surface-2 . "#282a36")
    (fg . "#f8f8f2")
    (muted . "#6272a4")
    (purple . "#bd93f9")
    (cyan . "#8be9fd")
    (pink . "#ff79c6")
    (orange . "#ffb86c")
    (red . "#ff6e6e")
    (green . "#5f9e8f")
    (rose . "#a45a6b")
    (added-bg . "#243b36")
    (removed-bg . "#3a2630")
    (refine-added . "#2f5a4a")
    (refine-removed . "#5a2f3a"))
  "OpenCode dark palette.")

(defun opencode-dark--color (name)
  "Return palette color NAME."
  (alist-get name opencode-dark--palette))

(defun opencode-dark-apply ()
  "Apply the OpenCode dark palette to the current buffer."
  (interactive)
  (when opencode-use-dark-theme
    (face-remap-add-relative
     'opencode-request-margin-highlight
     `(:foreground ,(opencode-dark--color 'cyan) :weight normal))
    (face-remap-add-relative
     'opencode-reasoning-margin-highlight
     `(:foreground ,(opencode-dark--color 'muted) :weight normal))
    (face-remap-add-relative
     'opencode-tool-margin-highlight
     `(:foreground ,(opencode-dark--color 'orange) :weight normal))
    (face-remap-add-relative
     'diff-added
     `(:foreground ,(opencode-dark--color 'green)
       :background ,(opencode-dark--color 'added-bg) :extend t))
    (face-remap-add-relative
     'diff-removed
     `(:foreground ,(opencode-dark--color 'rose)
       :background ,(opencode-dark--color 'removed-bg) :extend t))
    (face-remap-add-relative
     'diff-refine-added
     `(:foreground ,(opencode-dark--color 'fg)
       :background ,(opencode-dark--color 'refine-added)))
    (face-remap-add-relative
     'diff-refine-removed
     `(:foreground ,(opencode-dark--color 'fg)
       :background ,(opencode-dark--color 'refine-removed)))
    (face-remap-add-relative
     'diff-context `(:foreground ,(opencode-dark--color 'muted)))
    (face-remap-add-relative
     'markdown-ts-code `(:foreground ,(opencode-dark--color 'pink)))))

(defun opencode-dark-toggle ()
  "Toggle the OpenCode dark palette for the current buffer."
  (interactive)
  (setq opencode-use-dark-theme (not opencode-use-dark-theme))
  (if opencode-use-dark-theme
      (progn
        (opencode-dark-apply)
        (message "OpenCode dark theme enabled"))
    (face-remap-reset-base 'opencode-request-margin-highlight)
    (face-remap-reset-base 'opencode-reasoning-margin-highlight)
    (face-remap-reset-base 'opencode-tool-margin-highlight)
    (face-remap-reset-base 'diff-added)
    (face-remap-reset-base 'diff-removed)
    (face-remap-reset-base 'diff-refine-added)
    (face-remap-reset-base 'diff-refine-removed)
    (face-remap-reset-base 'diff-context)
    (face-remap-reset-base 'markdown-ts-code)
    (message "OpenCode dark theme disabled")))

(add-hook 'opencode-session-mode-hook #'opencode-dark-apply)

(provide 'opencode-theme)
;;; opencode-theme.el ends here
