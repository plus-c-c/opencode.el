;;; opencode-sessions-test.el --- Session rendering tests  -*- lexical-binding: t; -*-
;;; Code:

(require 'ert)
(require 'opencode-sessions)

(ert-deftest opencode-read-tool-file-location-button-test ()
  "Format read offsets as working file-and-line buttons."
  (let* ((file-directory (file-name-directory
                          (locate-library "opencode-sessions")))
         (default-directory file-directory)
         (formatted (opencode--format-tool-call
                     "read" '((filePath . "opencode-sessions.el")
                              (offset . 440)
                              (limit . 35)))))
    (with-temp-buffer
      (setq default-directory file-directory)
      (insert formatted)
      (opencode--buttonize-file-references (point-min) (point-max))
      (goto-char (point-min))
      (search-forward "opencode-sessions.el:440")
      (let ((button (button-at (1- (point)))))
        (should button)
        (should (equal (button-get button 'button-data)
                       (cons (expand-file-name "opencode-sessions.el"
                                                file-directory)
                             440)))))))

(ert-deftest opencode-session-markdown-fontification-test ()
  "Stream Markdown without replacing text or losing session decorations."
  (skip-unless (and (treesit-language-available-p 'markdown)
                    (treesit-language-available-p 'markdown-inline)))
  (let ((buffer (generate-new-buffer "opencode-markdown-test")))
    (unwind-protect
        (with-current-buffer buffer
          (setq default-directory (file-name-directory (locate-library "opencode-sessions")))
          (opencode-session-mode)
          (font-lock-mode 1)
          (let ((process (start-process "opencode-markdown-test" buffer nil)))
            (unwind-protect
                (progn
                  (opencode--output "# Heading\n\nSee `opencode-sessions")
                  (opencode--output ".el` and *emphasis*.\n\n")
                  (opencode--insert-tool-block
                   "edit" '((filePath . "opencode-sessions.el")
                            (oldString . "old") (newString . "new")))
                  (opencode--output "**Testing button behavior**\n\n")
                  (opencode--insert-block-with-margin
                   (concat "emacs_eval_elisp:\n"
                           (opencode--fenced-block "(+ 1 2)" "elisp"))
                   'opencode-tool-margin-highlight)
                  (opencode--maybe-insert-block-spacing)
                  (opencode--show-prompt)
                  (insert "pending input")
                  (let ((text (buffer-substring-no-properties (point-min) (point-max)))
                        (mark (marker-position (process-mark process)))
                        (position (point)))
                    (font-lock-ensure)
                    ;; Refontifying code blocks must not clear earlier inline
                    ;; Markdown, even with normal JIT font-lock enabled.
                    (font-lock-flush)
                    (font-lock-ensure)
                    (should (equal text (buffer-substring-no-properties
                                         (point-min) (point-max))))
                    (should (= mark (marker-position (process-mark process))))
                    (should (= position (point)))
                    (should (equal "pending input" (buffer-substring-no-properties
                                                    mark (point-max)))))
                  (save-excursion
                    (goto-char (point-min))
                    (search-forward "Heading")
                    (should (memq 'markdown-ts-heading-1
                                  (get-text-property (1- (point)) 'face)))
                    (goto-char (point-min))
                    (search-forward "opencode-sessions.el")
                    (let ((button (button-at (1- (point)))))
                      (should button)
                      (should (equal (car (button-get button 'button-data))
                                     (expand-file-name "opencode-sessions.el"))))
                    (goto-char (point-min))
                    (search-forward "-old")
                    (should (memq 'diff-removed
                                  (get-text-property (1- (point)) 'face)))
                    (should (cl-some (lambda (ov) (overlay-get ov 'line-prefix))
                                     (overlays-at (1- (point)))))
                    (search-forward "```")
                    (should-not (cl-some (lambda (ov) (overlay-get ov 'line-prefix))
                                         (overlays-at (- (point) 2))))
                    (search-forward "**Testing button behavior**")
                    (should (memq 'markdown-ts-bold
                                  (get-text-property (- (point) 27) 'face)))
                    (should (get-char-property (- (point) 27) 'invisible))
                    (search-forward "(+ 1 2)")
                    (should (cl-some (lambda (ov) (overlay-get ov 'line-prefix))
                                     (overlays-at (1- (point)))))
                    (search-forward "```")
                    (should-not (cl-some (lambda (ov) (overlay-get ov 'line-prefix))
                                         (overlays-at (- (point) 2))))
                    (goto-char (point-min))
                    (search-forward "> ")
                    (should (get-char-property (- (point) 2) 'invisible))))
              (delete-process process))))
      (kill-buffer buffer))))

(ert-deftest opencode-session-diff-refinement-test ()
  "Refine entire synthetic hunks without touching input or earlier output."
  (skip-unless (and (treesit-language-available-p 'markdown)
                    (treesit-language-available-p 'markdown-inline)))
  (with-temp-buffer
    (opencode-session-mode)
    (let ((process (start-process "opencode-refinement-test" (current-buffer) nil))
          (diff-refine 'font-lock))
      (unwind-protect
          (progn
            (opencode--output "Earlier output\n")
            (insert "pending input")
            (opencode--insert-tool-block
             "apply_patch"
             '((patchText . "*** Begin Patch\n*** Update File: example.el\n@@\n-old first value\n-old second value\n+new first value\n+new second value\n@@\n-before last value\n+after last value\n*** End Patch")))
            (let ((text (buffer-substring-no-properties (point-min) (point-max)))
                  (mark (marker-position (process-mark process)))
                  (position (point)))
              (font-lock-ensure)
              (font-lock-flush)
              (font-lock-ensure)
              (should (equal text (buffer-substring-no-properties (point-min) (point-max))))
              (should (= mark (process-mark process)))
              (should (= position (point)))
              (should (equal "pending input"
                             (buffer-substring-no-properties mark (point-max)))))
            (save-excursion
              (goto-char (point-min))
              (should-not (get-char-property (point) 'invisible))
              (dolist (change '(("old second" . diff-refine-removed)
                                ("new second" . diff-refine-added)
                                ("before last" . diff-refine-removed)
                                ("after last" . diff-refine-added)))
                (search-forward (car change))
                (should (cl-some
                         (lambda (overlay)
                           (eq (overlay-get overlay 'face) (cdr change)))
                         (overlays-at (- (point) (length (car change)))))))
              (goto-char (point-min))
              (while (search-forward "@@ -0 +0 @@" nil t)
                (should (get-char-property (- (point) 11) 'invisible)))))
        (delete-process process)))))

(ert-deftest opencode-session-yank-code-block-test ()
  "Copy only code from a fenced block, including embedded fences."
  (skip-unless (and (treesit-language-available-p 'markdown)
                    (treesit-language-available-p 'markdown-inline)))
  (with-temp-buffer
    (opencode-session-mode)
    (insert (opencode--fenced-block "```\ntext\n" "text") "\n")
    (goto-char (point-min))
    (search-forward "text" nil nil 2)
    (let ((node (treesit-node-at (point) 'markdown)))
      (while (and node (not (equal (treesit-node-type node) "fenced_code_block")))
        (setq node (treesit-node-parent node)))
      (should node))
    (opencode-yank-code-block)
    (should (equal (current-kill 0 t) "```\ntext\n"))))

(ert-deftest opencode-session-completed-tables-test ()
  "Align completed streamed tables once without disturbing pending input."
  (skip-unless (and (treesit-language-available-p 'markdown)
                    (treesit-language-available-p 'markdown-inline)))
  (with-temp-buffer
    (setq default-directory (file-name-directory (locate-library "opencode-sessions")))
    (opencode-session-mode)
    (let ((process (start-process "opencode-table-test" (current-buffer) nil))
          (opencode-session-buffers (make-hash-table :test 'equal))
          (opencode-part-type (make-hash-table :test 'equal))
          (opencode-assistant-messages (list (cons "message" nil)))
          (part '((sessionID . "session") (messageID . "message") (text . "")))
          (align (symbol-function 'opencode--align-output-table))
          (calls 0))
      (puthash "session" (current-buffer) opencode-session-buffers)
      (unwind-protect
          (cl-letf (((symbol-function 'opencode--align-output-table)
                     (lambda (beg end)
                       (cl-incf calls)
                       (funcall align beg end))))
            (insert "pending input")
            (opencode-session--update-part "text" part "| File | Value |\n" "text")
            (opencode-session--update-part "text" part "| :--- | ---: |\n" "text")
            (opencode-session--update-part
             "text" part "| `opencode-sessions.el` | 7 |\n" "text")
            (should (= calls 0))
            ;; An incomplete following line cannot yet establish the boundary.
            (opencode-session--update-part "text" part "# Following" "text")
            (should (= calls 0))
            (opencode-session--update-part "text" part " prose.\n" "text")
            (should (= calls 1))
            (let ((output (buffer-substring-no-properties
                           (point-min) (process-mark process))))
              (should (string-match-p "| +7|" output))
              (should (string-suffix-p "Following prose.\n" output)))
            (should (equal (buffer-substring-no-properties
                            (process-mark process) (point-max))
                           "pending input"))
            (should (= (point) (point-max)))
            (save-excursion
              (goto-char (point-min))
              (search-forward "opencode-sessions.el")
              (should (button-at (1- (point))))
              (search-forward "7|")
              (should (eq (get-text-property (- (point) 3) 'field) 'output)))
            ;; A fenced table is code and must never be aligned.
            (opencode-session--update-part
             "text" part "\n```markdown\n| Code | Value |\n| --- | --- |\n| x | y |\n```\n\n"
             "text")
            (should (= calls 1))
            ;; A blank line completes a table before the text part ends.
            (opencode-session--update-part
             "text" part "| Name | Value |\n| --- | --- |\n| x | y |\n\n" "text")
            (should (= calls 2))
            (opencode-session--update-part "text" part "More prose.\n\n" "text")
            (should (= calls 2))
            ;; Flush a final table without a trailing newline on part completion.
            (opencode-session--update-part
             "text" part "| Last | Value |\n| --- | --- |\n| x | y |" "text")
            (should (= calls 2))
            (opencode-session--update-part
             "text" (append '((time . ((end . 1)))) part) nil "text")
            (should (= calls 3))
            (should-not opencode--table-scan-marker)
            (should (eq (char-before (process-mark process)) ?|))
            (should (equal (buffer-substring-no-properties
                            (process-mark process) (point-max))
                           "pending input")))
        (delete-process process)))))

(ert-deftest opencode-session-table-alignment-context-test ()
  "Table alignment must not schedule a context mode in the session buffer."
  (skip-unless (and (treesit-language-available-p 'markdown)
                    (treesit-language-available-p 'markdown-inline)))
  (with-temp-buffer
    (opencode-session-mode)
    (insert "| Name | Value |\n| --- | --- |\n| x | y |\n")
    (let ((markdown-ts-enable-table-mode t)
          (markdown-ts-enable-code-block-context-mode t)
          (timer (symbol-function 'run-with-timer))
          context-timers)
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (seconds repeat function &rest args)
                   (if (memq function '(markdown-ts--enable-in-table-mode
                                        markdown-ts--enable-code-block-in-context-mode))
                       (push function context-timers)
                     (apply timer seconds repeat function args)))))
        (opencode--align-output-table (point-min) (point-max)))
      (should-not context-timers)
      (should-not markdown-ts-table-mode)
      (should-not markdown-ts-in-table-mode)
      (should (eq (key-binding (kbd "RET")) 'comint-send-input)))))

(ert-deftest opencode-session-history-table-alignment-test ()
  "Align replayed assistant tables, preserving user requests and code fences."
  (skip-unless (and (treesit-language-available-p 'markdown)
                    (treesit-language-available-p 'markdown-inline)))
  (with-temp-buffer
    (opencode-session-mode)
    (let* ((process (start-process "opencode-history-table-test" (current-buffer) nil))
           (table "| Name | Value |\n| --- | ---: |\n| x | y |")
           (fenced (concat "```markdown\n" table "\n```"))
           (messages
            `(((info . ((role . "user")))
               (parts . (((type . "text") (text . ,table)))))
              ((info . ((role . "assistant")
                        (tokens . ((input . 0) (output . 0) (reasoning . 0)
                                   (cache . ((read . 0) (write . 0)))))))
               (parts . (((type . "text") (text . ,table))
                         ((type . "text") (text . ,fenced))))))))
      (unwind-protect
          (progn
            ;; Capture the request separately to verify it reaches the user
            ;; renderer without going through the table alignment path.
            (cl-letf (((symbol-function 'opencode--replay-user-request)
                       (lambda (message)
                         (opencode--output (alist-get 'text (car (alist-get 'parts message))))
                         (opencode--output "\n\n"))))
              (opencode--replay-session-messages messages))
            (let ((output (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-prefix-p (concat table "\n\n") output))
              (should (string-match-p "|x +| +y|" output))
              (should (string-suffix-p (concat fenced "\n\n") output)))
            (should-not opencode--table-scan-marker)
            (should (= (process-mark process) (point-max)))
            (should-not markdown-ts-in-table-mode))
        (delete-process process)))))

(provide 'opencode-sessions-test)
;;; opencode-sessions-test.el ends here
