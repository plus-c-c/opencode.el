;;; opencode-question.el --- Question selection UI for opencode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Arthur Heymans

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Keywords: internal

;;; Commentary:

;; Provides a transient-based UI for answering questions from the opencode
;; server.  Supports single-select, multiple-select, and custom typed answers.
;;
;; Each option is a numbered key in the transient popup.  Press the key
;; to toggle the option.  Confirm with C-c C-c, reject with q.
;;
;; The transient directly calls the server API on confirm or reject,
;; avoiding `recursive-edit' which conflicts with transient key handling.

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-sessions)

;;; Per-question display state (global, since transient is not buffer-local)

(defvar opencode-question--marked nil
  "Vector of booleans tracking which options are marked.")

(defvar opencode-question--labels nil
  "Vector of label strings for the options.")

(defvar opencode-question--descriptions nil
  "Vector of description strings for the options.")

(defvar opencode-question--multiple nil
  "Non-nil if the current question allows multiple selections.")

(defvar opencode-question--custom-enabled nil
  "Non-nil if the current question allows a custom typed answer.")

(defvar opencode-question--custom-text nil
  "Custom text entered by the user, or nil if none.")

(defvar opencode-question--question-text nil
  "The full question text currently being displayed.")

;;; Batch state (tracks the full question sequence and accumulated answers)

(defvar opencode-question--question-id nil
  "The question ID for replying or rejecting via the server API.")

(defvar opencode-question--questions nil
  "The full vector of question objects being asked.")

(defvar opencode-question--current-index 0
  "Index of the current question being displayed.")

(defvar opencode-question--answers nil
  "Accumulated list of answer lists, one per answered question.")

(defvar opencode-question--handled nil
  "Non-nil when confirm or reject has been processed for the current transient.")

(defvar opencode-question--field-keys nil
  "Vector of form field keys corresponding to `opencode-question--questions'.")

(defvar opencode-question--option-values nil
  "Vector of vectors mapping option labels to V2 form values.")

;;; Helpers

(defun opencode-question--format-option (idx)
  "Format option IDX as a checkbox string with label and description."
  (let ((marked (aref opencode-question--marked idx))
        (label (aref opencode-question--labels idx))
        (desc (aref opencode-question--descriptions idx)))
    (concat (if marked "[x]" "[ ]") " " label
            (when desc
              (concat "  " (propertize desc 'face 'shadow))))))

(defun opencode-question--toggle-option (idx)
  "Toggle option at IDX, respecting single/multi-select mode."
  (if opencode-question--multiple
      (aset opencode-question--marked idx
            (not (aref opencode-question--marked idx)))
    ;; Single-select: radio button
    (let ((cur (aref opencode-question--marked idx)))
      (dotimes (i (length opencode-question--marked))
        (aset opencode-question--marked i nil))
      (unless cur
        (aset opencode-question--marked idx t)
        (setq opencode-question--custom-text nil)))))

(defun opencode-question--index-to-key (i)
  "Convert numeric index I to a transient key string."
  (cond ((< i 9) (string (+ ?1 i)))
        ((< i 35) (string (+ ?a (- i 9))))
        (t (format "M-%d" (- i 35)))))

(defun opencode-question--collect ()
  "Collect currently selected labels and custom text into a list."
  (let ((selected (cl-loop for i below (length opencode-question--labels)
                           when (aref opencode-question--marked i)
                           collect (aref opencode-question--labels i))))
    (when opencode-question--custom-text
      (setq selected (append selected (list opencode-question--custom-text))))
    selected))

;;; Custom suffix class for checkbox options

(defclass opencode-question-option (transient-suffix)
  ((option-index :initarg :option-index :initform 0)
   (transient :initform t))
  "A togglable checkbox option suffix for opencode questions.")

(cl-defmethod transient-format-description ((obj opencode-question-option))
  "Format OBJ as a checkbox line."
  (opencode-question--format-option (oref obj option-index)))

;;; Custom suffix class for typed answer

(defclass opencode-question-custom (transient-suffix)
  ((transient :initform t))
  "A custom text input option suffix for opencode questions.")

(cl-defmethod transient-format-description ((_obj opencode-question-custom))
  "Format the custom answer line with current text."
  (concat (if opencode-question--custom-text "[x]" "[ ]")
          " Type your own answer"
          (when opencode-question--custom-text
            (concat "  "
                    (propertize opencode-question--custom-text
                                'face 'font-lock-string-face)))))

;;; Suffix commands

(defun opencode-question--close-on-multiple ()
  "Exit transient in single-select, stay in multi-select."
  (if opencode-question--multiple
      (transient--do-stay)
    (transient--do-exit)))

(transient-define-suffix opencode-question--do-toggle ()
  "Toggle the checkbox option associated with this suffix."
  :class 'opencode-question-option
  :transient #'opencode-question--close-on-multiple
  (interactive)
  (opencode-question--toggle-option
   (oref (transient-suffix-object) option-index))
  (unless opencode-question--multiple
    (opencode-question--do-confirm)))

(transient-define-suffix opencode-question--do-custom ()
  "Type a custom answer."
  :class 'opencode-question-custom
  :transient #'opencode-question--close-on-multiple
  (interactive)
  (let ((text (read-string "Custom answer: "
                           opencode-question--custom-text)))
    (if (string-empty-p text)
        (setq opencode-question--custom-text nil)
      (setq opencode-question--custom-text text)
      (unless opencode-question--multiple
        ;; Single-select: clear all option marks
        (dotimes (i (length opencode-question--marked))
          (aset opencode-question--marked i nil))
        (opencode-question--do-confirm)))))

(defun opencode-question--confirm-pre-command ()
  "Pre-command function for confirm: exit if selection is non-empty, else stay."
  (if (opencode-question--collect)
      transient--exit
    transient--stay))

(defun opencode-question--current-values (selected)
  "Return V2 form values for SELECTED option labels."
  (let ((values (aref opencode-question--option-values
                      opencode-question--current-index)))
    (mapcar (lambda (label)
              (let ((index (cl-position label opencode-question--labels
                                        :test #'equal)))
                (if index (aref values index) label)))
            selected)))

(transient-define-suffix opencode-question--do-confirm ()
  "Confirm the current selection and reply to the server."
  :transient #'opencode-question--confirm-pre-command
  (interactive)
  (let ((selected (opencode-question--collect)))
    (if (not selected)
        (message "No options selected")
      (setq opencode-question--handled t)
      (let* ((index opencode-question--current-index)
             (key (if (and opencode-question--field-keys
                           (aref opencode-question--field-keys index))
                      (aref opencode-question--field-keys index)
                    (format "q%d" index)))
             (values (opencode-question--current-values selected))
             (value (if opencode-question--multiple
                        (vconcat values)
                      (car values))))
        (setf (alist-get key opencode-form--answers nil nil #'equal) value))
      (if (< (1+ opencode-question--current-index)
             (length opencode-question--questions))
          ;; More questions remain; advance after transient fully exits.
          (progn
            (cl-incf opencode-question--current-index)
            (run-at-time 0 nil #'opencode-question--show-current))
        ;; All questions answered; send the form reply.
        (when (and opencode-form--session-id opencode-form--form-id)
          (opencode-api-reply-form
              (opencode-form--session-id opencode-form--form-id)
              `((answer . ,opencode-form--answers))
              _result
            (setq opencode-form--active nil
                  opencode-form--form-id nil
                  opencode-form--session-id nil)
            (run-at-time 0 nil #'opencode-form--next)))))))

(transient-define-suffix opencode-question--do-reject ()
  "Reject the form and notify the server."
  :transient nil
  (interactive)
  (setq opencode-question--handled t)
  (opencode-form--cancel))

;;; Transient exit hook

(defun opencode-question--on-exit ()
  "Handle transient exit by rejecting if not already confirmed or rejected."
  (remove-hook 'transient-exit-hook #'opencode-question--on-exit)
  (unless opencode-question--handled
    (opencode-form--cancel)))

;;; Dynamic children setup

(defun opencode-question--setup-options (_children)
  "Generate checkbox suffix specs from current question state.
_CHILDREN is the statically-defined children, which are ignored."
  (transient-parse-suffixes
   'opencode-question--transient
   (append
    (cl-loop for i below (length opencode-question--labels)
             collect `(,(opencode-question--index-to-key i)
                       opencode-question--do-toggle
                       :option-index ,i))
    (when opencode-question--custom-enabled
      '(("!" opencode-question--do-custom))))))

;;; Transient prefix

(transient-define-prefix opencode-question--transient ()
  "Answer a question from the opencode server."
  [:class transient-column
   :description (lambda () (or opencode-question--question-text "Question"))
   :setup-children opencode-question--setup-options]
  [["Actions"
    ("C-c C-c" "Confirm" opencode-question--do-confirm :if (lambda () opencode-question--multiple))
    ("q"       "Reject"  opencode-question--do-reject)]])

;;; Entry points

(defun opencode-question--show-current ()
  "Set up per-question state and display transient for current question."
  (let* ((q (aref opencode-question--questions
                  opencode-question--current-index))
         (options (alist-get 'options q))
         (n (length options)))
    (setq opencode-question--labels (make-vector n nil)
          opencode-question--descriptions (make-vector n nil)
          opencode-question--option-values (make-vector n nil)
          opencode-question--marked (make-vector n nil)
          opencode-question--multiple (not (opencode--json-falsy
                                            (alist-get 'multiple q)))
          opencode-question--custom-enabled
          (if (assq 'custom q) (alist-get 'custom q) t)
          opencode-question--custom-text nil
          opencode-question--question-text
          (or (alist-get 'question q) (alist-get 'header q) "Question")
          opencode-question--handled nil)
    (dotimes (i n)
      (let* ((opt (aref options i))
             (label (alist-get 'label opt))
             (values (alist-get 'option-values q)))
        (aset opencode-question--labels i label)
        (aset opencode-question--descriptions i (alist-get 'description opt))
        (aset opencode-question--option-values i
              (if (and values (aref values i))
                  (aref values i)
                label))))
    (add-hook 'transient-exit-hook #'opencode-question--on-exit)
    (opencode-question--transient)))

(defun opencode-question--prompt (question-id questions)
  "Prompt user to answer QUESTIONS and reply or reject to QUESTION-ID.
QUESTIONS is a vector of question objects with `question', `header',
`options', and `multiple' fields.  Each option has `label' and
`description' fields.  Displays a transient popup for each question.
On confirm, replies to the server.  On reject or dismiss, rejects."
  (setq opencode-question--question-id question-id
        opencode-question--questions questions
        opencode-question--current-index 0
        opencode-question--answers nil
        opencode-question--handled nil)
  (opencode-question--show-current))

(defun opencode--prompt-questions (question-id questions)
  "Prompt user to answer QUESTIONS, then reply or reject to QUESTION-ID.
QUESTIONS is a vector of question objects with `question', `header',
`options', and `multiple' fields.  Each option has `label' and
`description' fields.  Displays a transient popup for each question.
On confirm, replies to the server.  On reject or dismiss, rejects."
  (opencode-question--prompt question-id questions))

(defun opencode--output-questions (buffer questions)
  "Output QUESTIONS to BUFFER with ❓ prefix for each question."
  (when (and (buffer-live-p buffer)
             (get-buffer-process buffer))
    (with-current-buffer buffer
      (opencode--maybe-insert-block-spacing)
      (opencode--output (opencode--format-questions questions))
      (opencode--output "\n"))))

(defun opencode--queue-questions (buffer question-id questions)
  "Queue QUESTION-ID with QUESTIONS in BUFFER and prompt if active."
  (opencode--output-questions buffer questions)
  (if (opencode--buffer-active-p buffer)
      (opencode--prompt-questions question-id questions)
    (opencode--toast-show `((title . "OpenCode Questions")
                            (message . ,(alist-get 'question (aref questions 0)))
                            (variant . "info")))
    (with-current-buffer buffer
      (setq opencode-session-pending-questions
            (cons question-id questions)))))

(defun opencode--question-request (question-id session-id questions)
  "Handle QUESTION-ID with QUESTIONS for SESSION-ID."
  (opencode-form--handle-question
   `((id . ,question-id)
     (sessionID . ,session-id)
     (questions . ,questions))))

(require 'opencode-form)

(provide 'opencode-question)
;;; opencode-question.el ends here
