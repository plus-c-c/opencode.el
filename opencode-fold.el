;;; opencode-fold.el --- Org-style folding for OpenCode output -*- lexical-binding: t; -*-

;;; Commentary:

;; OpenCode's TUI collapses reasoning and tool-detail blocks.  This file
;; provides the same experience using Org's `org-fold-core' engine while
;; keeping the session buffer's comint input handling intact.
;;
;; The first line of each reasoning/tool block stays visible as its
;; summary.  The rest of the block is folded under that line and can be
;; toggled with TAB, like an Org subtree.

;;; Code:

(require 'org-element)
(require 'org-fold-core)
(require 'seq)

(defgroup opencode-fold nil
  "Org-style folding for OpenCode session output."
  :group 'opencode)

(defcustom opencode-fold-thinking t
  "Whether new reasoning blocks start folded.
This corresponds to the TUI's collapsed thinking blocks."
  :type 'boolean
  :group 'opencode-fold)

(defcustom opencode-fold-tool-details t
  "Whether new tool-call blocks start folded.
This corresponds to the TUI's hidden tool details."
  :type 'boolean
  :group 'opencode-fold)

(defconst opencode--fold-spec 'opencode-fold
  "Org fold spec used for OpenCode blocks.")

(defvar-local opencode--fold-initialized nil)
(defvar-local opencode--fold-last-block nil
  "Metadata overlay for the most recently rendered OpenCode block.")

(defun opencode-fold-init ()
  "Initialize Org folding in the current OpenCode session buffer."
  (unless opencode--fold-initialized
    (setq opencode--fold-initialized t)
    (org-fold-core-initialize)
    (org-fold-core-add-folding-spec
     opencode--fold-spec
     '((:ellipsis . " …")
       (:isearch-open . t)))))

(defun opencode-fold-default-folded-p (type)
  "Return non-nil when a new block of TYPE should start folded."
  (pcase type
    ('reasoning opencode-fold-thinking)
    ('tool opencode-fold-tool-details)
    (_ nil)))

(defun opencode-fold--body-start (overlay)
  "Return the body start marker of fold OVERLAY."
  (overlay-get overlay 'opencode-fold-body-start))

(defun opencode-fold--body-end (overlay)
  "Return the body end marker of fold OVERLAY."
  (overlay-get overlay 'opencode-fold-body-end))

(defun opencode-fold--sync-margin (overlay folded)
  "Keep OVERLAY's left margin bar the same height as its visible text.
When FOLDED, the bar ends at the summary line; otherwise it spans the
whole block."
  (let ((margin (overlay-get overlay 'opencode-fold-margin-overlay)))
    (when (and (overlayp margin) (overlay-buffer margin))
      (let* ((start (overlay-start overlay))
             (body-start (marker-position (opencode-fold--body-start overlay)))
             (end (if folded
                      (max (1+ start) (1- body-start))
                    (1- (overlay-end overlay)))))
        (when (> end start)
          (move-overlay margin start end))))))

(defun opencode-fold-apply (overlay folded)
  "Fold or unfold OVERLAY's body according to FOLDED."
  (let ((from (marker-position (opencode-fold--body-start overlay)))
        (to (marker-position (opencode-fold--body-end overlay))))
    (when (and from to (< from to))
      (org-fold-core-region from to folded opencode--fold-spec))
    (overlay-put overlay 'opencode-fold-folded folded)
    (opencode-fold--sync-margin overlay folded)))

(defun opencode-fold-register (start end type &optional margin-overlay)
  "Register a foldable block from START to END as TYPE.
TYPE is `reasoning' or `tool'.  The first line is kept as the visible
summary and the remaining lines are folded according to the matching
customization.  MARGIN-OVERLAY is the left-bar overlay for the block."
  (when (and (< start end)
             (memq type '(reasoning tool)))
    (let* ((last opencode--fold-last-block)
           (header-end (save-excursion
                         (goto-char start)
                         (line-end-position)))
           (body-start (copy-marker header-end))
           (body-end (copy-marker end t)))
      (if (and last
               (eq (overlay-get last 'opencode-fold-type) type)
               (= start (overlay-end last)))
          ;; A streaming reasoning/text block.  Extend the existing
          ;; summary instead of creating one fold per delta.
          (progn
            (when (and (overlayp margin-overlay)
                       (overlayp (overlay-get last 'opencode-fold-margin-overlay)))
              (delete-overlay margin-overlay))
            (unless (overlay-get last 'opencode-fold-margin-overlay)
              (when (overlayp margin-overlay)
                (overlay-put last 'opencode-fold-margin-overlay margin-overlay)))
            (move-overlay last (overlay-start last) end)
            (set-marker (opencode-fold--body-end last) end)
            (if (overlay-get last 'opencode-fold-folded)
                (opencode-fold-apply last t)
              (opencode-fold--sync-margin last nil))
            last)
        (let ((overlay (make-overlay start end)))
          (overlay-put overlay 'opencode-fold-block t)
          (overlay-put overlay 'opencode-fold-type type)
          (overlay-put overlay 'opencode-fold-body-start body-start)
          (overlay-put overlay 'opencode-fold-body-end body-end)
          (overlay-put overlay 'opencode-fold-margin-overlay margin-overlay)
          (overlay-put overlay 'opencode-fold-folded nil)
          (setq opencode--fold-last-block overlay)
          (when (opencode-fold-default-folded-p type)
            (opencode-fold-apply overlay t))
          overlay)))))

(defun opencode-fold-extend (type start end)
  "Extend the last TYPE block to include text from START to END."
  (let ((last opencode--fold-last-block))
    (when (and last
               (eq (overlay-get last 'opencode-fold-type) type)
               (= start (overlay-end last)))
      (move-overlay last (overlay-start last) end)
      (set-marker (opencode-fold--body-end last) end)
      (if (overlay-get last 'opencode-fold-folded)
          (opencode-fold-apply last t)
        (opencode-fold--sync-margin last nil))
      last)))

(defun opencode-fold-overlay-at-point ()
  "Return the OpenCode fold overlay at point, if any."
  (or (seq-find (lambda (overlay)
                  (overlay-get overlay 'opencode-fold-block))
                (overlays-at (point)))
      (seq-find (lambda (overlay)
                  (overlay-get overlay 'opencode-fold-block))
                (overlays-at (line-beginning-position)))))

(defun opencode-fold-toggle-at-point ()
  "Toggle the fold at point.  Return non-nil when a fold was found."
  (interactive)
  (when-let* ((overlay (opencode-fold-overlay-at-point)))
    (opencode-fold-apply overlay
                         (not (overlay-get overlay 'opencode-fold-folded)))
    t))

(defun opencode-fold-toggle-all ()
  "Toggle all OpenCode reasoning/tool blocks in the current buffer."
  (interactive)
  (let ((overlays (seq-filter
                   (lambda (overlay)
                     (overlay-get overlay 'opencode-fold-block))
                   (overlays-in (point-min) (point-max)))))
    (let ((target (not (seq-some
                        (lambda (overlay)
                          (overlay-get overlay 'opencode-fold-folded))
                        overlays))))
      (dolist (overlay overlays)
        (opencode-fold-apply overlay target)))))

(defun opencode-fold-set-type (type folded)
  "Set all TYPE blocks to FOLDED and update the default for new blocks."
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (eq (overlay-get overlay 'opencode-fold-type) type)
      (opencode-fold-apply overlay folded)))
  (pcase type
    ('reasoning (setq opencode-fold-thinking folded))
    ('tool (setq opencode-fold-tool-details folded))))

(defun opencode-fold-toggle-thinking ()
  "Toggle all reasoning blocks, matching the TUI thinking toggle."
  (interactive)
  (opencode-fold-set-type 'reasoning (not opencode-fold-thinking)))

(defun opencode-fold-toggle-tool-details ()
  "Toggle all tool-call blocks, matching the TUI details toggle."
  (interactive)
  (opencode-fold-set-type 'tool (not opencode-fold-tool-details)))

(defun opencode-fold-refresh ()
  "Create folds for reasoning/tool blocks already in the buffer.
This is useful for buffers opened before folding was enabled."
  (interactive)
  (let ((candidates
         (seq-filter
          (lambda (overlay)
            (let* ((prefix (overlay-get overlay 'line-prefix))
                   (display (and (stringp prefix)
                                 (get-text-property 0 'display prefix)))
                   (margin (and (listp display)
                                (seq-find #'stringp display)))
                   (face (and margin
                              (get-text-property 0 'face margin))))
              (memq face '(opencode-reasoning-margin-highlight
                           opencode-tool-margin-highlight))))
          (overlays-in (point-min) (point-max)))))
    (dolist (overlay (sort candidates
                            (lambda (a b)
                              (< (overlay-start a) (overlay-start b)))))
      (let* ((prefix (overlay-get overlay 'line-prefix))
             (display (get-text-property 0 'display prefix))
             (margin (seq-find #'stringp display))
             (face (get-text-property 0 'face margin))
             (type (if (eq face 'opencode-reasoning-margin-highlight)
                       'reasoning
                     'tool)))
        (opencode-fold-register (overlay-start overlay)
                                (overlay-end overlay)
                                type overlay)))))

(provide 'opencode-fold)
;;; opencode-fold.el ends here
