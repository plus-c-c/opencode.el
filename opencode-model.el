;;; opencode-model.el --- Model and agent selection -*- lexical-binding: t; -*-

;;; Commentary:

;; Providers, models, variants and session agents.

;;; Code:

(require 'opencode-api)
(require 'opencode-common)
(require 'subr-x)

(defvar opencode-session-id)
(defvar opencode-session-agent)
(defvar opencode-session-agents)
(defvar opencode-providers)
(defvar opencode-last-model)

(defun opencode--agent-id (agent)
  "Return the API id for AGENT."
  (or (alist-get 'id agent)
      (alist-get 'name agent)))


(defun opencode-cycle-session-agent ()
  "Switch to the next agent in `opencode-session-agents'."
  (interactive)
  (let* ((name (opencode--agent-id opencode-session-agent))
         (agents opencode-session-agents)
         (pos (and name (cl-position-if
                         (lambda (agent)
                           (equal name (opencode--agent-id agent)))
                         agents))))
    (when pos
      (let ((next (seq-find
                   (lambda (agent)
                     (member (alist-get 'mode agent) '("primary" "all")))
                   (append (nthcdr (1+ pos) agents)
                           (seq-take agents (1+ pos))))))
        (when next
          (if opencode-session-id
              (opencode-api-set-session-agent
                  (opencode-session-id)
                  `((agent . ,(opencode--agent-id next)))
                  _res
                (setq opencode-session-agent next)
                (force-mode-line-update))
            (setq opencode-session-agent next)
            (force-mode-line-update))))))
  (force-mode-line-update))


(defun opencode--collect-all-models ()
  "Collect all models from `opencode-providers' as a list.
Each element is (display-name . (provider-id provider-name model-id))."
  (let (result)
    (dolist (provider opencode-providers)
      (let-alist provider
        (dolist (model-entry .models)
          (let ((model (cdr model-entry)))
            (push (list (alist-get 'name model)
                        `((providerID . ,.id)
                          (modelID . ,(alist-get 'id model)))
                        .name)
                  result)))))
    (nreverse result)))


(defun opencode--model-ref ()
  "Return the V2 model reference for the current session agent."
  (let-alist opencode-session-agent
    (when .model
      `((id . ,(or .model.modelID .model.id))
        (providerID . ,.model.providerID)
        ,@(when .variant `((variant . ,.variant)))))))


(defun opencode--push-session-model ()
  "Tell the V2 server about the buffer's selected model and variant."
  (when (and opencode-session-id (opencode--model-ref))
    (opencode-api-set-session-model
        (opencode-session-id)
        `((model . ,(opencode--model-ref)))
        _res
      (force-mode-line-update))))


(defun opencode-select-model ()
  "Select a model for the current session and agent using completion."
  (interactive)
  (unless opencode-session-agent
    (user-error "not in a session"))
  (when-let ((model (opencode--annotated-completion
                     "Model: "
                     (opencode--collect-all-models))))
    (setf (alist-get 'model opencode-session-agent) model
          opencode-last-model model)
    (when-let ((variant (alist-get 'variant opencode-session-agent)))
      (unless (assoc-string variant
                            (alist-get 'variants (opencode--current-model)))
        (setq opencode-session-agent
              (assq-delete-all 'variant opencode-session-agent))))
    (opencode--push-session-model)))


(defun opencode--current-model ()
  "Return the active model for this session."
  (let-alist opencode-session-agent
    (when (and .model.providerID .model.modelID)
      (map-nested-elt
       (seq-find (lambda (provider)
                   (string= .model.providerID (alist-get 'id provider)))
                 opencode-providers)
       `(models ,(intern (or .model.modelID .model.id)))))))


(defun opencode-select-variant ()
  "Select a variant for the current model."
  (interactive)
  (unless opencode-session-agent
    (user-error "not in a session"))
  (let ((variants (alist-get 'variants (opencode--current-model))))
    (if variants
        (when-let ((variant
                    (opencode--annotated-completion
                     "Variant: "
                     (cl-loop for (variant . options) in variants
                              for name = (if (symbolp variant)
                                             (symbol-name variant)
                                           variant)
                              collect (list name name (format "%s" options))))))
          (setf (alist-get 'variant opencode-session-agent) variant)
          (opencode--push-session-model))
      (message "No variants"))))


(provide 'opencode-model)
;;; opencode-model.el ends here
