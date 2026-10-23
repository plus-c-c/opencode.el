;;; opencode-api.el --- Wrapper for opencode api     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Wrapper for the OpenCode V2 HTTP API.  The V2 server exposes its API
;; below /api and wraps most responses in a `data' member.  The
;; normalization functions in opencode-v2.el convert those responses to
;; the shapes the rest of this package already understands.

;;; Code:

(require 'opencode-common)
(require 'opencode-v2)
(require 'plz)
(require 'json)
(require 'subr-x)

(defvar opencode-api-url nil
  "URL of the opencode server we're connected to.")

(defvar opencode-api-version nil
  "Version reported by the connected opencode server.")

(defvar opencode-api-log-max-lines nil
  "Maximum number of lines of opencode api requests and responses to log.
Or nil (default) to turn off logging.")

(defun opencode--auth-header ()
  "Return auth header based on `opencode-server-username' and `opencode-server-password'."
  (when opencode-server-password
    (cons "Authorization"
          (concat "Basic "
                  (base64-encode-string
                   (format "%s:%s" opencode-server-username opencode-server-password))))))

(defvar opencode-api-normalizers
  '((projects . opencode-v2--projects)
    (current-project . opencode-v2--current-project)
    (configured-providers . opencode-v2--providers)
    (session . opencode-v2--session)
    (session-children . opencode-v2--sessions)
    (session-messages . opencode-v2--session-messages)
    (sessions . opencode-v2--sessions)
    (sessions-status . opencode-v2--active-status)
    (create-session . opencode-v2--session)
    (fork-session . opencode-v2--session)
    (abort-session . opencode-v2--interrupted)
    (agents . opencode-v2--agents)
    (commands . opencode-v2--commands)
    (skills . opencode-v2--skills)
    (mcps . opencode-v2--mcps)
    (session-forms . opencode-v2--data)
    (forms . opencode-v2--data))
  "Normalizers applied to responses from the OpenCode V2 API.")

(eval-and-compile
  (cl-defun opencode-api--call (method path return-var body
                                      &key data name normalizer)
    "Generate the body of an opencode api wrapper macro.
Using METHOD, for endpoint PATH, saving result in RETURN-VAR,
and saving to CURRENT-BUFFER while running BODY."
    (cl-with-gensyms (current-buffer result saved-path saved-data)
      `(let ((,current-buffer (current-buffer))
             (,saved-path ,path)
             (,saved-data ,data))
         (when opencode-api-log-max-lines
           (with-current-buffer (get-buffer-create "*opencode-api-log*")
             (save-excursion
               (goto-char (point-max))
               (insert "REQUEST: " ,saved-path "\n")
               (when ,saved-data
                 (insert "REQUEST BODY:")
                 (pp ,saved-data (current-buffer)))
               (opencode--truncate-at-max-lines opencode-api-log-max-lines))))
         (plz ',method (concat opencode-api-url ,saved-path)
           :as (lambda ()
                 (goto-char (point-min))
                 (unless (string-empty-p (buffer-string))
                   (json-parse-buffer :array-type 'list
                                      :object-type 'alist)))
           :headers (delq nil
                          (list (cons "Content-Type" "application/json")
                                (cons "Accept" "application/json")
                                (cons "x-opencode-directory"
                                      (expand-file-name default-directory))
                                (opencode--auth-header)))
           ,@(when data
               `(:body (json-encode ,saved-data)))
           :then (lambda (,result)
                   (when opencode-api-log-max-lines
                     (with-current-buffer
                         (get-buffer-create "*opencode-api-log*")
                       (save-excursion
                         (goto-char (point-max))
                         (insert "RESPONSE: ")
                         (pp ,result (current-buffer)))))
                   (let ((,return-var
                          (let ((raw ,result))
                            (if-let ((normalizer
                                      (or ',normalizer
                                          (alist-get ',name
                                                     opencode-api-normalizers))))
                                (funcall normalizer raw)
                              raw))))
                     (if (buffer-live-p ,current-buffer)
                         (with-current-buffer ,current-buffer
                           ,@body)
                       (message "Buffer closed so opencode api response handler not run"))))
           :else (lambda (response)
                   (let* ((body (and (plz-response-p response)
                                     (plz-response-body response)))
                          (decoded (ignore-errors
                                     (and body
                                          (not (string-empty-p body))
                                          (json-parse-string
                                           body :object-type 'alist))))
                          (error-msg
                           (format "error requesting %s: %s"
                                   ,saved-path
                                   (or (alist-get 'message decoded)
                                       decoded
                                       response))))
                     (when opencode-api-log-max-lines
                       (with-current-buffer
                           (get-buffer-create "*opencode-api-log*")
                         (save-excursion
                           (goto-char (point-max))
                           (insert "ERROR: " error-msg "\n"))))
                     (if opencode--event-subscription
                         (error "%s" error-msg)
                       (error "Not connected to opencode"))))))))

  (cl-defun opencode-api--wrap (method path
                                       &key elisp-macro-name nodata normalizer)
    "Define a macro to wrap api call with METHOD and PATH.
Optionally provide ELISP-MACRO-NAME where opencode-api-elisp-macro-name will be
the name of the created macro.  Set NODATA to indicate a request doesn't have a
body when it normally would (POST PATCH).  NORMALIZER is applied to the response."
    (let ((elisp-macro-name
           (intern
            (concat "opencode-api"
                    (if elisp-macro-name
                        (concat "-" (symbol-name elisp-macro-name))
                      (string-replace "/" "-" path))))))
      (eval
       (if (seq-contains-p path ?\%)
           (if (and (seq-contains-p '(post patch) method)
                    (not nodata))
               `(defmacro ,elisp-macro-name
                    (args data return-var &rest body)
                  ,(format "Wrapper for opencode %s endpoint." path)
                  (declare (indent 3))
                  (opencode-api--call ',method `(format ,,path ,@args) return-var body
                                      :data data :name ',elisp-macro-name
                                      :normalizer ',normalizer))
             `(defmacro ,elisp-macro-name
                  (args return-var &rest body)
                ,(format "Wrapper for opencode %s endpoint." path)
                (declare (indent 2))
                (opencode-api--call ',method `(format ,,path ,@args) return-var body
                                    :name ',elisp-macro-name
                                    :normalizer ',normalizer)))
         (if (and (seq-contains-p '(post patch) method)
                  (not nodata))
             `(defmacro ,elisp-macro-name
                  (data return-var &rest body)
                ,(format "Wrapper for opencode %s endpoint." path)
                (declare (indent 2))
                (opencode-api--call ',method ,path return-var body
                                    :data data :name ',elisp-macro-name
                                      :normalizer ',normalizer))
           `(defmacro ,elisp-macro-name
                (return-var &rest body)
              ,(format "Wrapper for opencode %s endpoint." path)
              (declare (indent defun))
              (opencode-api--call ',method ,path return-var body
                                  :name ',elisp-macro-name
                                  :normalizer ',normalizer)))))))

  (defun opencode-api-define-wrapper (endpoint)
    "Create a macro wrapping ENDPOINT."
    (let ((method 'get)
          nodata)
      (when (and (listp endpoint)
                 (seq-contains-p '(get patch post delete) (car endpoint)))
        (setf method (pop endpoint))
        (when (eq 'nodata (car endpoint))
          (pop endpoint)
          (setf nodata t)))
      (pcase endpoint
        ((pred stringp)
         (opencode-api--wrap method endpoint :nodata nodata))
        (`(,elisp-name ,path)
         (opencode-api--wrap method path
                             :elisp-macro-name elisp-name
                             :nodata nodata))
        (`(,elisp-name ,path ,normalizer)
         (opencode-api--wrap method path
                             :elisp-macro-name elisp-name
                             :nodata nodata
                             :normalizer normalizer)))))

  (defun opencode-api-define-wrappers (api-endpoints)
    "Define wrapping macros for a list of API-ENDPOINTS."
    (dolist (api-endpoint api-endpoints)
      (opencode-api-define-wrapper api-endpoint)))

  (opencode-api-define-wrappers
   '((info "/api/info")
     (projects "/api/project" opencode-v2--projects)
     (current-project "/api/location" opencode-v2--current-project)
     (configured-providers "/api/model" opencode-v2--providers)
     (providers "/api/provider")
     (session "/api/session/%s" opencode-v2--session)
     (session-children "/api/session?parentID=%s&limit=200" opencode-v2--sessions)
     (session-diff "/api/session/%s/diff")
     (session-messages "/api/session/%s/message?limit=200&order=desc"
                       opencode-v2--session-messages)
     (post send-message "/api/session/%s/prompt")
     (message-details "/api/session/%s/message/%s")
     (sessions "/api/session?limit=50&directory=%s" opencode-v2--sessions)
     (session-search "/api/session?limit=200&search=%s&directory=%s"
                     opencode-v2--sessions)
     (sessions-status "/api/session/active" opencode-v2--active-status)
     (post create-session "/api/session" opencode-v2--session)
     (patch rename-session "/api/session/%s")
     (post nodata abort-session "/api/session/%s/interrupt"
           opencode-v2--interrupted)
     (delete delete-session "/api/session/%s")
     (post fork-session "/api/session/%s/fork" opencode-v2--session)
     (post revert-message "/api/session/%s/revert/stage")
     (post nodata commit-revert "/api/session/%s/revert/commit")
     (delete unrevert-all "/api/session/%s/revert")
     (post respond-permission-request "/api/session/%s/permission/%s/reply")
     (permission-saved "/api/permission/saved" opencode-v2--data)
     (post execute-command "/api/session/%s/command")
     (post execute-shell "/api/session/%s/shell")
     (post summarize "/api/session/%s/compact")
     (post set-session-model "/api/session/%s/model")
     (post set-session-agent "/api/session/%s/agent")
     (session-forms "/api/session/%s/form" opencode-v2--data)
     (stats "/api/experimental/session/stats" opencode-v2--data)
     (export-session "/api/experimental/session/%s/export" opencode-v2--data)
     (post import-session "/api/experimental/session/import" opencode-v2--session)
     (post create-form "/api/session/%s/form")
     (post reply-form "/api/session/%s/form/%s/reply")
     (delete cancel-form "/api/session/%s/form/%s")
     (forms "/api/form" opencode-v2--data)
     (commands "/api/command" opencode-v2--commands)
     (skills "/api/skill" opencode-v2--skills)
     (mcps "/api/mcp" opencode-v2--mcps)
     (post nodata enable-mcp "/api/experimental/mcp/%s/connect")
     (post nodata disable-mcp "/api/experimental/mcp/%s/disconnect")
     (agents "/api/agent" opencode-v2--agents))))

(provide 'opencode-api)
;;; opencode-api.el ends here
