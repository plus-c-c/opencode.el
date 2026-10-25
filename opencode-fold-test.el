;;; opencode-fold-test.el --- Org folding integration tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'opencode-render)
(require 'opencode-fold)

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

(provide 'opencode-fold-test)
;;; opencode-fold-test.el ends here
