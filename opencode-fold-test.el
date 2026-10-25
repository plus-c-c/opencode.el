;;; opencode-fold-test.el --- Org folding integration tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'opencode-render)
(require 'opencode-fold)
(require 'opencode-sessions)

(ert-deftest opencode-fold-margin-sync-test ()
  "Folding and unfolding keeps the left bar in sync with visible text."
  (with-temp-buffer
    (insert "Summary\nbody one\nbody two\n")
    (opencode-fold-init)
    (let* ((start (point-min))
           (end (point-max))
           (header-end (save-excursion
                         (goto-char start)
                         (line-end-position)))
           (margin (opencode--add-margin start end 'opencode-tool-margin-highlight))
           (fold (opencode-fold-register start end 'tool margin)))
      (should (overlay-get fold 'opencode-fold-folded))
      (should (= (overlay-end margin) (1- header-end)))
      (goto-char start)
      (should (opencode-fold-toggle-at-point))
      (should-not (overlay-get fold 'opencode-fold-folded))
      (should (= (overlay-end margin) (1- end)))
      (should (opencode-fold-toggle-at-point))
      (should (overlay-get fold 'opencode-fold-folded))
      (should (= (overlay-end margin) (1- header-end))))))

(ert-deftest opencode-reasoning-stream-header-test ()
  "Reasoning stays as a header while the body is accumulated out of sight."
  (skip-unless (and (treesit-language-available-p 'markdown)
                    (treesit-language-available-p 'markdown-inline)))
  (with-temp-buffer
    (opencode-session-mode)
    (let ((process (start-process "opencode-reason-test" (current-buffer) nil)))
      (unwind-protect
          (progn
            (opencode--start-reasoning)
            (should (equal (buffer-substring-no-properties (point-min) (point-max))
                           "Thinking\n"))
            (opencode--insert-reasoning-delta "alpha\nbeta\n")
            (should (equal (buffer-substring-no-properties (point-min) (point-max))
                           "Thinking\n"))
            (opencode--finish-reasoning)
            (should (string-prefix-p "Thought\nalpha\nbeta\n"
                                     (buffer-substring-no-properties
                                      (point-min) (point-max)))))
        (delete-process process)))))

(provide 'opencode-fold-test)
;;; opencode-fold-test.el ends here
