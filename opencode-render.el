;;; opencode-render.el --- Rendering helpers for OpenCode sessions -*- lexical-binding: t; -*-

;;; Commentary:

;; Comint output, Markdown, tables, margins, diffs and tool blocks.

;;; Code:

(require 'opencode-common)
(require 'opencode-format-tool-calls)
(require 'opencode-diff-parser)

(defun opencode--fontify-code-block (mode beg end)
  "Collect MODE faces for BEG..END without sharing session text properties."
  (let ((text (buffer-substring-no-properties beg end))
        faces)
    (with-temp-buffer
      (insert text)
      (delay-mode-hooks (funcall mode))
      (font-lock-ensure)
      (let ((pos (point-min)))
        (while (< pos (point-max))
          (let ((next (next-single-property-change pos 'face nil (point-max)))
                (face (get-text-property pos 'face)))
            (when face
              (push (list (+ beg (1- pos)) (+ beg (1- next)) face) faces))
            (setq pos next)))))
    (nreverse faces)))


(defun opencode--fontify-markdown (start end &optional loudly)
  "Fontify Markdown between START and END, optionally LOUDLY.
Isolate conventional code-block fontification from session properties."
  (let ((collect (symbol-function 'markdown-ts--fontify-non-ts-collect-faces)))
    (cl-letf (((symbol-function 'markdown-ts--fontify-non-ts-collect-faces)
               (lambda (mode beg finish)
                 (if (memq mode markdown-ts-code-block-force-conventional-modes)
                     (funcall collect mode beg finish)
                   (opencode--fontify-code-block mode beg finish)))))
      (treesit-font-lock-fontify-region start end loudly))))


(defun opencode--maybe-insert-block-spacing ()
  "Ensure \n\n before block."
  (let ((pos (opencode--session-process-position)))
    (opencode--output
     (cond
      ((not (eq ?\n (char-before pos))) "\n\n")
      ((eq ?\n (char-before pos))
       (if (eq ?\n (char-before (1- pos))) "" "\n"))
      (t "")))))


(defun opencode--output (string)
  "Output STRING as comint output."
  (let ((start (save-excursion
                 (goto-char (opencode--session-process-position))
                 (let ((inhibit-field-text-motion t))
                   (line-beginning-position)))))
    (comint-output-filter (get-buffer-process (current-buffer)) string)
    (opencode--buttonize-file-references
     start (opencode--session-process-position))))


(defun opencode--align-output-table (beg end)
  "Align the completed Markdown table between BEG and END.
Use Markdown's table command in an isolated buffer, then apply only its
text changes so session properties and markers survive on unchanged text."
  (let ((source (current-buffer))
        (text (buffer-substring-no-properties beg end))
        (output-properties
         (cl-loop for property in '(field read-only insert-in-front-hooks
                                         inhibit-line-move-field-capture front-sticky)
                  for value = (get-text-property beg property)
                  when value append (list property value)))
        (inhibit-read-only t))
    (save-excursion
      (with-temp-buffer
        ;; Automatic context modes schedule timers which run in whichever
        ;; buffer is current later, potentially overriding the session's RET.
        ;; Enable only the table context needed by the alignment command.
        (let ((markdown-ts-enable-table-mode nil)
              (markdown-ts-enable-code-block-context-mode nil))
          (delay-mode-hooks (markdown-ts-mode)))
        (insert text)
        (goto-char (point-min))
        (markdown-ts-in-table-mode 1)
        (markdown-ts-table-align-table t)
        ;; The command always adds a newline.  Do not introduce one if the
        ;; final text part ended at the last cell instead.
        (unless (string-suffix-p "\n" text)
          (when (eq (char-before (point-max)) ?\n)
            (delete-region (1- (point-max)) (point-max))))
        (let ((aligned (current-buffer)))
          (with-current-buffer source
            (save-restriction
              (narrow-to-region beg end)
              (replace-buffer-contents aligned)
              ;; Diff insertion preserves existing cells, but newly inserted
              ;; padding also needs comint's output field and protection.
              (when output-properties
                (let ((pos (point-min)))
                  (while (< pos (point-max))
                    (let ((next (next-single-property-change pos 'field nil (point-max))))
                      (unless (get-text-property pos 'field)
                        (add-text-properties pos next output-properties))
                      (setq pos next)))))
              (opencode--buttonize-file-references (point-min) (point-max)))))))))


(defun opencode--align-finished-tables (&optional finished)
  "Align tables whose end is confirmed by subsequent complete output lines.
If FINISHED is non-nil, also align a table at the end of the text part.
Retain one line of context for headers whose delimiter has yet to arrive."
  (when opencode--table-scan-marker
    (save-excursion
      (save-restriction
        ;; Pending comint input is not part of the assistant's Markdown.
        (narrow-to-region (point-min) (opencode--session-process-position))
        (let* ((inhibit-field-text-motion t)
               (limit (copy-marker (opencode--session-process-position)))
               (complete-end
                (copy-marker
                 (if finished
                     limit
                   (goto-char limit)
                   (line-beginning-position))))
               waiting)
          (unwind-protect
              (progn
                (goto-char opencode--table-scan-marker)
                (while (and (< (point) complete-end) (not waiting))
                  (let ((node (treesit-node-at (point) 'markdown)))
                    (while (and node (not (equal (treesit-node-type node) "pipe_table")))
                      (setq node (treesit-node-parent node)))
                    ;; At whitespace, `treesit-node-at' can return the table
                    ;; immediately before point.  Never revisit that table.
                    (if (and node (< (point) (treesit-node-end node)))
                        (let ((beg (treesit-node-start node))
                              (end (treesit-node-end node)))
                          (if (and (<= end limit)
                                   (or finished
                                       (< (save-excursion
                                            (goto-char end)
                                            (if (bolp) (point)
                                              (line-beginning-position 2)))
                                          complete-end)))
                              (let ((next (copy-marker end t)))
                                (unwind-protect
                                    (progn
                                      (opencode--align-output-table beg end)
                                      (goto-char next)
                                      (set-marker opencode--table-scan-marker next))
                                  (set-marker next nil)))
                            (set-marker opencode--table-scan-marker beg)
                            (setq waiting t)))
                      (set-marker opencode--table-scan-marker (point))
                      (forward-line 1))))
                (when finished
                  (set-marker opencode--table-scan-marker nil)
                  (setq opencode--table-scan-marker nil)))
            (set-marker limit nil)
            (set-marker complete-end nil)))))))


(defun opencode-insert-logo ()
  "Insert the opencode logo."
  (let ((logo-left '("                   " "█▀▀█ █▀▀█ █▀▀█ █▀▀▄"
                     "█░░█ █░░█ █▀▀▀ █░░█" "▀▀▀▀ █▀▀▀ ▀▀▀▀ ▀  ▀"))
        (logo-right '("             ▄     " "█▀▀▀ █▀▀█ █▀▀█ █▀▀█"
                      "█░░░ █░░█ █░░█ █▀▀▀" "▀▀▀▀ ▀▀▀▀ ▀▀▀▀ ▀▀▀▀")))
    (cl-loop for left in logo-left
             for right in logo-right
             do (opencode--output (propertize left 'face 'shadow))
             (opencode--output " ")
             (opencode--output (propertize right 'face 'bold))
             (opencode--output "\n"))
    (opencode--output "\n")))


(defun opencode--show-prompt ()
  "Highlight the prompt after displaying output.
Make sure a response that did not end with a newline cannot leave its
last line attached to the user's input line."
  (let ((start (opencode--session-process-position)))
    (unless (or (bobp) (eq (char-before start) ?\n))
      (opencode--output "\n"))
    (setq start (opencode--session-process-position))
    (opencode--output "> ")
    (let ((ov (make-overlay start (opencode--session-process-position))))
      (overlay-put ov 'invisible t)))
  (opencode--add-margin (car comint-last-prompt)
                        (cdr comint-last-prompt)
                        'opencode-request-margin-highlight))


(defface opencode-request-margin-highlight
  '((t :inherit outline-1 :height reset))
  "OpenCode margin face to apply to user requests."
  :group 'opencode-faces)


(defface opencode-reasoning-margin-highlight
  '((t :inherit outline-2 :height reset))
  "OpenCode margin face to apply to reasoning blocks."
  :group 'opencode-faces)


(defface opencode-tool-margin-highlight
  '((t :inherit outline-5 :height reset))
  "OpenCode margin face to apply to tool call blocks."
  :group 'opencode-faces)


(defun opencode--add-margin (start end face)
  "Display margin from START (inclusive) to END (exclusive) with FACE."
  (let ((ov (make-overlay start (1- end)))
        (margin (propertize ">" 'display
                            `((margin left-margin)
                              ,(propertize "▎" 'face face)))))
    (overlay-put ov 'line-prefix margin)
    (overlay-put ov 'wrap-prefix margin)))


(defun opencode--insert-block-with-margin (text face)
  "Insert TEXT with FACE margin highlight."
  (unless (string-empty-p text)
    (let ((beginning (opencode--session-process-position)))
      (opencode--output text)
      (opencode--add-margin beginning (save-excursion
                                        (goto-char (opencode--session-process-position))
                                        (skip-chars-backward "\r\n[:blank:]")
                                        ;; Markdown hides closing fences, so
                                        ;; their otherwise visible margin would
                                        ;; appear as an extra highlighted line.
                                        (let ((inhibit-field-text-motion t))
                                          (beginning-of-line))
                                        (unless (looking-at-p "[`~]\\{3,\\}[[:blank:]]*$")
                                          (goto-char (opencode--session-process-position))
                                          (skip-chars-backward "\r\n[:blank:]"))
                                        (point))
                            face))))


(defun opencode--insert-reasoning-block (text)
  "Insert TEXT as reasoning block."
  (opencode--insert-block-with-margin text 'opencode-reasoning-margin-highlight))


(defun opencode--session-process-position ()
  "Return position of process marker."
  (marker-position
   (process-mark
    (get-buffer-process
     (current-buffer)))))


(defun opencode--refine-diff-hunks (start)
  "Refine and hide diff hunk headers from START to the process marker."
  (let ((inhibit-read-only t)
        (inhibit-field-text-motion t))
    (save-excursion
      (save-restriction
        (narrow-to-region start (opencode--session-process-position))
        (goto-char (point-min))
        (while (re-search-forward diff-hunk-header-re nil t)
          (let* ((beg (match-beginning 0))
                 (header-end (line-beginning-position 2))
                 (end (progn
                        (goto-char beg)
                        ;; apply_patch headers have synthetic line counts.
                        (diff-end-of-hunk nil t)
                        (point))))
            (when diff-refine
              (diff--refine-hunk beg end t))
            ;; Font-lock can clear invisible text properties on refontification.
            (let ((overlay (make-overlay beg header-end)))
              (overlay-put overlay 'invisible t)
              (overlay-put overlay 'evaporate t))
            (goto-char (max header-end end))))))))


(defun opencode--insert-tool-block (tool input)
  "Insert TOOL call with INPUT as margin-highlighted block."
  (let ((start (opencode--session-process-position)))
    (opencode--insert-block-with-margin
     (opencode--format-tool-call tool input)
     'opencode-tool-margin-highlight)
    (opencode--maybe-insert-block-spacing)
    (when (member tool '("edit" "apply_patch"))
      (opencode--refine-diff-hunks start))))


(defun opencode--maybe-insert-tool-output (message)
  "Maybe insert the output from tool call in MESSAGE."
  (let-alist message
    (when (and (string= .state.status "completed")
               (or opencode-show-tool-output
                   ;; show output for user run shell commands
                   (and (member .tool '("bash" "shell"))
                        (not .state.input.description)))
               .state.output)
      (opencode--output (opencode--fenced-block .state.output "text"))
      (opencode--output "\n"))))


(provide 'opencode-render)
;;; opencode-render.el ends here
