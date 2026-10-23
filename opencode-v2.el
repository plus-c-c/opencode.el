;;; opencode-v2.el --- OpenCode V2 response normalization -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; The V2 server API has a different shape from the V1 API that the rest
;; of this package was originally written against.  This file converts V2
;; responses into the small set of V1-shaped values the UI already knows
;; how to display.  Keeping the conversion here means the rendering and
;; comint code does not need to know which server version produced a value.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun opencode-v2--data (result)
  "Return the `data' member of RESULT, or RESULT itself for bare arrays."
  (if (and (listp result) (assoc 'data result))
      (alist-get 'data result)
    result))

(defun opencode-v2--session (session)
  "Normalize a V2 SESSION into the shape used by opencode.el."
  (when session
    (setq session (opencode-v2--data session))
    (let* ((location (alist-get 'location session))
           (directory (and location (alist-get 'directory location)))
           (tokens (or (alist-get 'tokens session)
                       '((input . 0) (output . 0) (reasoning . 0)
                         (cache . ((read . 0) (write . 0))))))
           (model (alist-get 'model session)))
      (when model
        (setf (alist-get 'model session)
              (append model
                      `((modelID . ,(or (alist-get 'modelID model)
                                        (alist-get 'id model)))))))
      (append session
              `((directory . ,directory)
                (tokens . ,tokens)
                (share . nil))))))

(defun opencode-v2--sessions (result)
  "Normalize a V2 session list RESULT."
  (delq nil (mapcar #'opencode-v2--session
                    (or (opencode-v2--data result) '()))))

(defun opencode-v2--project (project)
  "Normalize a V2 PROJECT."
  (when project
    (append project
            `((worktree . ,(or (alist-get 'canonical project)
                               (alist-get 'directory project)))))))

(defun opencode-v2--projects (result)
  "Normalize a V2 project list RESULT."
  (delq nil (mapcar #'opencode-v2--project
                    (or (opencode-v2--data result) '()))))

(defun opencode-v2--current-project (result)
  "Normalize the V2 location RESULT into a project with a worktree."
  (let* ((directory (alist-get 'directory result))
         (project (alist-get 'project result)))
    (when project
      (append (opencode-v2--project project)
              `((directory . ,directory))))))

(defun opencode-v2--agents (result)
  "Return the agent array from a V2 RESULT."
  (or (opencode-v2--data result) '()))

(defun opencode-v2--commands (result)
  "Return the command array from a V2 RESULT."
  (or (opencode-v2--data result) '()))

(defun opencode-v2--skills (result)
  "Return the skill array from a V2 RESULT."
  (or (opencode-v2--data result) '()))

(defun opencode-v2--mcps (result)
  "Normalize the V2 MCP list RESULT to the V1 alist shape.
Each element is (NAME . ((status . STATUS) (name . NAME)))."
  (let (servers)
    (dolist (server (or (opencode-v2--data result) '()))
      (let* ((name (alist-get 'name server))
             (status (alist-get 'status (alist-get 'status server))))
        (when name
          (push (cons (intern name)
                      `((status . ,status)
                        (name . ,name)))
                servers))))
    (nreverse servers)))

(defun opencode-v2--variants (variants)
  "Convert V2 VARIANTS (an array of objects) to the V1 alist shape."
  (cl-loop for variant in variants
           for id = (alist-get 'id variant)
           when id
           collect (cons (intern id) (alist-get 'settings variant))))

(defun opencode-v2--model (model)
  "Normalize a single V2 MODEL."
  (let ((copy (copy-tree model)))
    (setf (alist-get 'id copy) (alist-get 'modelID copy)
          (alist-get 'variants copy)
          (opencode-v2--variants (alist-get 'variants copy)))
    copy))

(defun opencode-v2--providers (result)
  "Build the V1 provider/model tree from the flat V2 model RESULT."
  (let ((models-by-provider (make-hash-table :test #'equal))
        (provider-order nil))
    (dolist (model (or (opencode-v2--data result) '()))
      (let ((provider-id (alist-get 'providerID model)))
        (when provider-id
          (unless (gethash provider-id models-by-provider)
            (push provider-id provider-order))
          (push (cons (intern (or (alist-get 'modelID model)
                                  (alist-get 'id model)))
                      (opencode-v2--model model))
                (gethash provider-id models-by-provider)))))
    `((providers
       . ,(cl-loop for provider-id in (nreverse provider-order)
                   collect `((id . ,provider-id)
                             (name . ,provider-id)
                             (models . ,(nreverse
                                         (gethash provider-id
                                                  models-by-provider)))))))))

(defun opencode-v2--tool-output (content)
  "Return the textual output from a V2 tool CONTENT array."
  (when content
    (string-join
     (delq nil
           (mapcar (lambda (item)
                     (pcase (alist-get 'type item)
                       ("text" (alist-get 'text item))
                       (_ nil)))
                   content))
     "\n")))

(defun opencode-v2--part (part)
  "Normalize one V2 assistant content PART."
  (pcase (alist-get 'type part)
    ("text"
     `((type . "text")
       (text . ,(or (alist-get 'text part) ""))
       (time . ,(alist-get 'time part))))
    ("reasoning"
     `((type . "reasoning")
       (text . ,(or (alist-get 'text part) ""))
       (time . ,(alist-get 'time part))))
    ("tool"
     (let* ((state (copy-tree (alist-get 'state part)))
            (status (alist-get 'status state))
            (output (opencode-v2--tool-output (alist-get 'content state))))
       (when state
         (setf (alist-get 'status state)
               (if (equal status "streaming") "running" status)
               (alist-get 'output state) output))
       `((type . "tool")
         (tool . ,(alist-get 'name part))
         (callID . ,(alist-get 'id part))
         (state . ,state)
         (time . ,(alist-get 'time part)))))
    (_ nil)))

(defun opencode-v2--message (message)
  "Normalize a V2 MESSAGE to the V1 (INFO . PARTS) representation.
Return nil for message types the UI does not render."
  (pcase (alist-get 'type message)
    ("user"
     (let ((parts nil)
           (text (alist-get 'text message)))
       (when (and text (not (string-empty-p text)))
         (push `((type . "text") (text . ,text)) parts))
       (dolist (file (alist-get 'files message))
         (push `((type . "file")
                 (filename . ,(alist-get 'name file))
                 (mime . ,(alist-get 'mime file))
                 (url . ,(alist-get 'uri file)))
               parts))
       (dolist (agent (alist-get 'agents message))
         (push `((type . "agent") (name . ,(alist-get 'name agent))) parts))
       (list (cons 'info
                   `((id . ,(alist-get 'id message))
                     (sessionID . ,(alist-get 'sessionID message))
                     (role . "user")
                     (time . ,(alist-get 'time message))))
             (cons 'parts (nreverse parts)))))
    ("assistant"
     (let* ((tokens (or (alist-get 'tokens message)
                        '((input . 0) (output . 0) (reasoning . 0)
                          (cache . ((read . 0) (write . 0))))))
            (parts (delq nil (mapcar #'opencode-v2--part
                                     (alist-get 'content message)))))
       (list (cons 'info
                   `((id . ,(alist-get 'id message))
                     (sessionID . ,(alist-get 'sessionID message))
                     (role . "assistant")
                     (time . ,(alist-get 'time message))
                     (tokens . ,tokens)
                     (model . ,(alist-get 'model message))
                     (agent . ,(alist-get 'agent message))
                     (finish . ,(alist-get 'finish message))
                     (error . ,(alist-get 'error message))))
             (cons 'parts parts))))
    (_ nil)))

(defun opencode-v2--session-messages (result)
  "Normalize a V2 session-message page RESULT."
  (delq nil (mapcar #'opencode-v2--message
                    (or (opencode-v2--data result) '()))))

(defun opencode-v2--status (status)
  "Convert a V2 STATUS object to the V1 busy/idle/retry string."
  (let ((type (if (listp status) (alist-get 'type status) status)))
    (pcase type
      ((or "running" "busy") "busy")
      ("idle" "idle")
      ("retry" "retry")
      (_ type))))

(defun opencode-v2--active-status (result)
  "Normalize the V2 active-session map RESULT."
  (cl-loop for (session-id . status) in (or (opencode-v2--data result) '())
           collect (cons (if (symbolp session-id)
                             (symbol-name session-id)
                           session-id)
                         `((type . ,(opencode-v2--status status))))))

(defun opencode-v2--interrupted (result)
  "Return non-nil when a V2 interrupt RESULT reports an interruption."
  (and (listp result) (eq t (alist-get 'interrupted result))))

(defun opencode-v2--tool-file (tool input)
  "Return the file affected by TOOL with INPUT, if it can be determined."
  (when (listp input)
    (pcase tool
      ((or "write" "edit" "apply_patch")
       (or (alist-get 'filePath input)
           (alist-get 'path input)
           (alist-get 'file input)))
      (_ nil))))

(provide 'opencode-v2)
;;; opencode-v2.el ends here
