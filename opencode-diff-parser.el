;;; opencode-diff-parser.el --- Parse diff edit ranges -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Return start and end lines of all edits in a unified diff.
;;
;; It computes post-edit line numbers directly from unified diff hunk
;; headers:
;;
;;   @@ -OLD-START,OLD-COUNT +NEW-START,NEW-COUNT @@
;;
;; Return value:
;;
;;   ((FILE . ((START-LINE . END-LINE)
;;             ...))
;;    ...)
;;
;; For deletion-only edits, END-LINE is nil:
;;
;;   (FILE . ((80 . nil)))
;;
;; That means the deleted text no longer exists in the post-edit file;
;; line 80 is the insertion/deletion point in the new file.

;;; Code:

(require 'cl-lib)

(defun opencode--git-unquote-file-name (file)
  "Return FILE with Git-style C quoting decoded if present.

For example, turn \"\\\"b/foo\\\\tbar.el\\\"\" into \"b/foo<TAB>bar.el\".
If FILE is not a quoted Git path, return it unchanged."
  (setq file (string-trim file))
  (if (and (> (length file) 0)
           (= (aref file 0) ?\"))
      (condition-case nil
          (let* ((parsed (read-from-string file))
                 (value (car parsed))
                 (end (cdr parsed)))
            (if (and (stringp value)
                     (= end (length file)))
                value
              file))
        (error file))
    file))

(defun opencode--diff-header-file-name (line prefix)
  "Return the file name from diff header LINE beginning with PREFIX.
Decode Git quoting if present.

PREFIX should be either \"--- \" or \"+++ \".

Returns nil for /dev/null."
  (when (string-prefix-p prefix line)
    (let* ((raw (substring line (length prefix)))
           ;; GNU diff often separates timestamps with a tab.  Do not split on
           ;; a plain space, because file names may contain spaces.
           (without-timestamp (car (split-string raw "\t")))
           (file (opencode--git-unquote-file-name
                  (string-trim without-timestamp))))
      (unless (string= "/dev/null" file)
        file))))

(defun opencode--parse-unified-hunk-header (line)
  "Parse unified diff hunk header LINE.

Return a plist:

  (:old-start OLD-START
   :old-count OLD-COUNT
   :new-start NEW-START
   :new-count NEW-COUNT)

Return nil if LINE is not a supported unified diff hunk header."
  (when (string-match
         "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
         line)
    (let ((old-start (string-to-number (match-string 1 line)))
          (old-count (if (match-string 2 line)
                         (string-to-number (match-string 2 line))
                       1))
          (new-start (string-to-number (match-string 3 line)))
          (new-count (if (match-string 4 line)
                         (string-to-number (match-string 4 line))
                       1)))
      (list :old-start old-start
            :old-count old-count
            :new-start new-start
            :new-count new-count))))

(defun opencode--add-range-to-table (table file start end)
  "Add range (START . END) for FILE to TABLE."
  (when (and file start)
    (push (cons start end)
          (gethash file table))))

(defun opencode--merge-line-ranges (ranges)
  "Merge adjacent non-deletion line RANGES.

Deletion-only ranges look like (LINE . nil) and are left alone."
  (let (out last)
    (dolist (range ranges)
      (if (and last
               (cdr last)
               (cdr range)
               (= (1+ (cdr last)) (car range)))
          (setcdr last (cdr range))
        (let ((cell (cons (car range) (cdr range))))
          (push cell out)
          (setq last cell))))
    (nreverse out)))

(defun opencode--table->alist (table)
  "Convert TABLE of FILE -> reversed ranges to the desired alist shape."
  (let (result)
    (maphash
     (lambda (file ranges)
       (push (cons file
                   (opencode--merge-line-ranges
                    (nreverse ranges)))
             result))
     table)
    (nreverse result)))

(defun opencode--diff-no-newline-marker-p (line)
  "Return non-nil if LINE is a diff no-newline marker."
  (string-prefix-p "\\ No newline at end of file" line))

(defun opencode--finish-hunk-if-complete
    (flush-delete-run hunk-old-left hunk-new-left)
  "Maybe finish the current hunk.

Call FLUSH-DELETE-RUN first if both HUNK-OLD-LEFT and HUNK-NEW-LEFT
are zero or less.

Return non-nil if the hunk is complete."
  (when (and (<= hunk-old-left 0)
             (<= hunk-new-left 0))
    (funcall flush-delete-run)
    t))

(defun opencode-diff->source-line-ranges (diff-text)
  "Return post-edit source line ranges described by DIFF-TEXT.

Return value:

  ((FILE . ((START-LINE . END-LINE)
            ...))
   ...)

For deletion-only edits, END-LINE is nil:

  (FILE . ((80 . nil)))

That means the deleted text no longer exists in the post-edit file;
line 80 is the insertion/deletion point in the new file."
  (let ((table (make-hash-table :test #'equal))
        current-file
        in-hunk

        ;; Current line counters inside a hunk.
        old-line
        new-line

        ;; Remaining old/new source lines in the current hunk.  These make the
        ;; parser robust to blank context lines represented as bare empty lines.
        hunk-old-left
        hunk-new-left

        ;; For detecting deletion-only edit runs.
        pending-delete-location
        pending-delete-had-addition)

    (cl-labels
        ((flush-delete-run
           ()
           (when (and pending-delete-location
                      (not pending-delete-had-addition))
             (pcase-let ((`(,file . ,line) pending-delete-location))
               (opencode--add-range-to-table table file line nil)))
           (setq pending-delete-location nil
                 pending-delete-had-addition nil))

         (finish-hunk-if-complete
           ()
           (when (opencode--finish-hunk-if-complete
                  #'flush-delete-run hunk-old-left hunk-new-left)
             (setq in-hunk nil
                   old-line nil
                   new-line nil
                   hunk-old-left nil
                   hunk-new-left nil)))

         (begin-hunk
           (header)
           (flush-delete-run)
           (setq in-hunk t
                 old-line (plist-get header :old-start)
                 new-line (plist-get header :new-start)
                 hunk-old-left (plist-get header :old-count)
                 hunk-new-left (plist-get header :new-count)))

         (consume-context-line
           ()
           ;; A context line separates edit runs.
           (flush-delete-run)
           (cl-incf old-line)
           (cl-incf new-line)
           (cl-decf hunk-old-left)
           (cl-decf hunk-new-left)
           (finish-hunk-if-complete))

         (consume-added-line
           ()
           ;; An added line exists in the post-edit file.
           (when pending-delete-location
             (setq pending-delete-had-addition t))
           (opencode--add-range-to-table table current-file new-line new-line)
           (cl-incf new-line)
           (cl-decf hunk-new-left)
           (finish-hunk-if-complete))

         (consume-removed-line
           ()
           ;; A removed line does not exist in the post-edit file.  Save the
           ;; current post-edit location in case this turns out to be a
           ;; deletion-only edit run.
           (unless pending-delete-location
             (setq pending-delete-location
                   (cons current-file (max 1 new-line))))
           (cl-incf old-line)
           (cl-decf hunk-old-left)
           (finish-hunk-if-complete)))

      (with-temp-buffer
        (insert diff-text)
        (goto-char (point-min))

        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))
                 (header (opencode--parse-unified-hunk-header line)))
            (cond
             ;; Inside a hunk, hunk-body lines take precedence over headers.
             ;; This matters for source lines that literally start with +++ or
             ;; ---; they are content if we are already inside a hunk.
             ((and in-hunk
                   (opencode--diff-no-newline-marker-p line))
              ;; This marker consumes neither old nor new lines and should not
              ;; split an edit run.
              nil)

             ((and in-hunk
                   (string-prefix-p "+" line))
              (consume-added-line))

             ((and in-hunk
                   (string-prefix-p "-" line))
              (consume-removed-line))

             ((and in-hunk
                   (string-prefix-p " " line))
              (consume-context-line))

             ((and in-hunk
                   (string-empty-p line))
              ;; Strict unified diffs represent an unchanged blank line as a
              ;; single leading space.  Some producers or string processing
              ;; paths may turn that into a bare empty line.  Treat it as a
              ;; context blank line so we do not accidentally leave the hunk.
              (consume-context-line))

             (header
              (begin-hunk header))

             ((string-prefix-p "+++ " line)
              (flush-delete-run)
              (setq current-file
                    (opencode--diff-header-file-name line "+++ "))
              (setq in-hunk nil))

             ((string-prefix-p "--- " line)
              ;; Old-file header.  We do not use it for post-edit ranges.
              (flush-delete-run)
              (setq current-file nil)
              (setq in-hunk nil))

             (t
              ;; File separators, Index lines, diff --git lines, malformed
              ;; lines, etc.
              (flush-delete-run)
              (setq in-hunk nil)))

            (forward-line 1))))

      (flush-delete-run))

    (opencode--table->alist table)))

(provide 'opencode-diff-parser)
;;; opencode-diff-parser.el ends here
