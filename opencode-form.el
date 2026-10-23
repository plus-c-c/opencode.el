;;; opencode-form.el --- OpenCode V2 forms -*- lexical-binding: t; -*-

;;; Commentary:

;; V2 forms replace the V1 question reply API.  This module collects form
;; fields, answers them through the transient question UI, and submits the
;; result to /api/session/{id}/form/{id}/reply.

;;; Code:

(require 'opencode-api)
(require 'opencode-common)
(require 'cl-lib)

(defvar opencode-question--field-keys)
(defvar opencode-question--questions)
(defvar opencode-question--current-index)
(defvar opencode-question--handled)
(defvar opencode-question--question-id)
(declare-function opencode-question--show-current "opencode-question")
(declare-function opencode-question--transient "opencode-question" ())

(defvar opencode-form--session-id nil
  "Session id for the form currently being answered.")


(defvar opencode-form--form-id nil
  "Form id for the form currently being answered.")


(defvar opencode-form--answers nil
  "Alist of field key to answer value for the current form.")


(defvar opencode-form--queue nil
  "Forms waiting to be shown.")


(defvar opencode-form--active nil
  "The form currently being shown, or nil.")


(defvar opencode-form--seen (make-hash-table :test 'equal)
  "Form ids that have already been queued this Emacs session.")


(defun opencode-form--cancel ()
  "Cancel the form currently being answered."
  (when (and opencode-form--session-id opencode-form--form-id)
    (opencode-api-cancel-form (opencode-form--session-id
                               opencode-form--form-id)
        _result
      nil))
  (setq opencode-form--active nil
        opencode-form--form-id nil
        opencode-form--session-id nil)
  (run-at-time 0 nil #'opencode-form--next))


(defun opencode-form--field-options (field)
  "Return a vector of question options for FIELD."
  (let* ((options (alist-get 'options field))
         (values (make-vector (length options) nil))
         (result (make-vector (length options) nil)))
    (cl-loop for option in options
             for index from 0
             for value = (alist-get 'value option)
             for label = (or (alist-get 'label option) value)
             do (aset values index value)
             do (aset result index
                      `((label . ,label)
                        (description . ,(alist-get 'description option)))))
    (list result values)))


(defun opencode-form--prompt-field (field)
  "Prompt for a non-option FIELD and return its value."
  (let* ((type (alist-get 'type field))
         (title (or (alist-get 'title field)
                    (alist-get 'key field)
                    "Value"))
         (description (alist-get 'description field))
         (prompt (if description
                     (format "%s (%s): " title description)
                   (format "%s: " title)))
         (default (alist-get 'default field)))
    (pcase type
      ("boolean" (y-or-n-p (concat prompt " ")))
      ((or "number" "integer")
       (read-number prompt (if (numberp default) default nil)))
      ("multiselect"
       (let* ((options (alist-get 'options field))
              (choices (mapcar (lambda (option)
                                 (cons (or (alist-get 'label option)
                                           (alist-get 'value option))
                                       (alist-get 'value option)))
                               options))
              (selected (completing-read-multiple
                         prompt (mapcar #'car choices) nil nil)))
         (vconcat (mapcar (lambda (label)
                            (or (cdr (assoc-string label choices)) label))
                          selected))))
      ("string"
       (let* ((options (alist-get 'options field))
              (choices (mapcar (lambda (option)
                                 (cons (or (alist-get 'label option)
                                           (alist-get 'value option))
                                       (alist-get 'value option)))
                               options)))
         (if options
             (let* ((labels (mapcar #'car choices))
                    (custom (alist-get 'custom field))
                    (answer (completing-read
                             prompt
                             (if custom (append labels '("Other...")) labels)
                             nil (alist-get 'required field))))
               (if (equal answer "Other...")
                   (read-string prompt)
                 (or (cdr (assoc-string answer choices)) answer)))
           (let ((answer (read-string prompt nil nil
                                      (and (stringp default) default))))
             (if (and (string-empty-p answer)
                      (not (alist-get 'required field)))
                 nil
               answer)))))
      (_ nil))))


(defun opencode-form--submit ()
  "Submit the current form answers."
  (when (and opencode-form--session-id opencode-form--form-id)
    (opencode-api-reply-form
        (opencode-form--session-id opencode-form--form-id)
        `((answer . ,opencode-form--answers))
        _result
      (setq opencode-form--active nil
            opencode-form--form-id nil
            opencode-form--session-id nil)
      (run-at-time 0 nil #'opencode-form--next))))


(defun opencode-form--show (form)
  "Show FORM and collect its answers."
  (setq opencode-form--form-id (alist-get 'id form)
        opencode-form--session-id (alist-get 'sessionID form)
        opencode-form--answers nil
        opencode-question--field-keys nil
        opencode-question--questions nil)
  (catch 'opencode-form-cancel
    (dolist (field (alist-get 'fields form))
      (unless (or (alist-get 'hidden field)
                  (equal (alist-get 'type field) "external"))
        (let ((type (alist-get 'type field))
              (options (alist-get 'options field)))
          (if (and options
                   (member type '("string" "multiselect")))
              nil
            (let ((value (opencode-form--prompt-field field)))
              (when (or value (alist-get 'required field))
                (setf (alist-get (alist-get 'key field)
                                 opencode-form--answers nil nil #'equal)
                      value)))))))
    (let (questions keys)
      (dolist (field (alist-get 'fields form))
        (unless (or (alist-get 'hidden field)
                    (equal (alist-get 'type field) "external"))
          (let ((options (alist-get 'options field))
                (type (alist-get 'type field)))
            (when (and options (member type '("string" "multiselect")))
              (let* ((option-data (opencode-form--field-options field))
                     (question
                      `((question . ,(or (alist-get 'description field)
                                         (alist-get 'title field)
                                         (alist-get 'key field)))
                        (header . ,(or (alist-get 'title field)
                                       (alist-get 'key field)))
                        (options . ,(car option-data))
                        (option-values . ,(cadr option-data))
                        (multiple . ,(equal type "multiselect"))
                        (custom . ,(alist-get 'custom field)))))
                (push question questions)
                (push (alist-get 'key field) keys))))))
      (setq questions (vconcat (nreverse questions))
            keys (vconcat (nreverse keys)))
      (if (zerop (length questions))
          (opencode-form--submit)
        (setq opencode-question--questions questions
              opencode-question--field-keys keys
              opencode-question--current-index 0
              opencode-question--handled nil
              opencode-question--question-id opencode-form--form-id)
        (opencode-question--show-current)))))


(defun opencode-form--next ()
  "Show the next queued form, if any."
  (unless opencode-form--active
    (when opencode-form--queue
      (let ((form (pop opencode-form--queue)))
        (setq opencode-form--active form)
        (condition-case nil
            (opencode-form--show form)
          (quit
           (opencode-form--cancel)))))))


(defun opencode-form--handle (form)
  "Queue FORM for display and answer."
  (let ((id (alist-get 'id form)))
    (unless (gethash id opencode-form--seen)
      (puthash id t opencode-form--seen)
      (setq opencode-form--queue
            (append opencode-form--queue (list form)))
      (opencode-form--next))))


(defun opencode-form--questions->fields (questions)
  "Convert V1 question objects QUESTIONS to V2 form fields."
  (cl-loop for question across (vconcat questions)
           for index from 0
           collect `((key . ,(format "q%d" index))
                     (title . ,(or (alist-get 'header question) "Question"))
                     (description . ,(alist-get 'question question))
                     (type . ,(if (opencode--json-falsy
                                    (alist-get 'multiple question))
                                  "string" "multiselect"))
                     (options . ,(cl-loop for option across
                                          (vconcat (alist-get 'options question))
                                          collect `((value . ,(alist-get 'label option))
                                                    (label . ,(alist-get 'label option))
                                                    (description . ,(alist-get 'description option)))))
                     (custom . t))))


(defun opencode-form--handle-question (data)
  "Handle a V2 question event DATA by showing its pending form."
  (let ((session-id (alist-get 'sessionID data))
        (question-id (alist-get 'id data)))
    (opencode-api-session-forms (session-id)
        forms
      (if-let* ((form (seq-find (lambda (form)
                                 (or (equal (alist-get 'id form) question-id)
                                     (equal (alist-get 'id (alist-get 'metadata form))
                                            question-id)))
                               forms)))
          (opencode-form--handle form)
        (opencode-form--handle
         `((id . ,question-id)
           (sessionID . ,session-id)
           (title . "Questions")
           (fields . ,(opencode-form--questions->fields
                       (alist-get 'questions data)))))))))

(provide 'opencode-form)
;;; opencode-form.el ends here
