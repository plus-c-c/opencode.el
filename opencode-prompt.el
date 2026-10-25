;;; opencode-prompt.el --- Prompt input and attachments -*- lexical-binding: t; -*-

;;; Commentary:

;; Comint input, file/buffer/region attachments and prompt conversion.

;;; Code:

(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-render)
(require 'cl-lib)

(defvar opencode-session-id)
(defvar opencode-session-agent)
(defvar opencode--extra-parts)
(defvar opencode--temp-files)

(defun opencode--highlight-input (&optional _proc _string)
  "Highlight last prompt input."
  (opencode--clear-input-margin)
  (opencode--add-margin comint-last-input-start
                        comint-last-input-end
                        'opencode-request-margin-highlight))


(defun opencode--mimetype (file)
  "Return guess of mimetype for FILE."
  (pcase (file-name-extension file)
    ((or "org" "ts" (pred null)) "text/plain")
    (ext (or (mailcap-extension-to-mime ext)
             "text/plain"))))


(defun opencode--remove-label (overlay after _beg _end &optional _len)
  "Called AFTER deleting OVERLAY, remove the associated part from context."
  (when (and after
             (not (eq this-command 'comint-send-input)))
    (let ((file-url (overlay-get overlay 'file-url))
          (buffer-name (overlay-get overlay 'buffer-name))
          (region-id (overlay-get overlay 'region-id))
          (agent-name (overlay-get overlay 'agent-name))
          (skill-id (overlay-get overlay 'skill-id))
          (ov-start (overlay-start overlay))
          (ov-end (overlay-end overlay)))
      (setq opencode--extra-parts
            (seq-remove (lambda (part)
                          (let-alist part
                            (cond
                             (file-url (string= file-url .url))
                             (buffer-name (string= buffer-name .metadata.buffer-name))
                             (region-id (eq region-id .metadata.region-id))
                             (agent-name (string= agent-name .name))
                             (skill-id (equal skill-id (alist-get 'id part))))))
                        opencode--extra-parts))
      (delete-region ov-start ov-end)
      (delete-overlay overlay))))


(defun opencode--in-label-p ()
  "Return non-nil if point is inside (or at the edge of) a label overlay."
  (cl-loop for ov in (overlays-at (point))
           thereis (overlay-get ov 'opencode-label)))


(defun opencode--insert-intangible (name extra-prop extra-value)
  "Insert an intangible label with NAME (buffer or filename).
Assign the overlay EXTRA-PROP with EXTRA-VALUE."
  (let* ((start (point))
         (end (progn (insert "`")
                     (insert (propertize name 'cursor-intangible t))
                     (insert (propertize "`" 'rear-nonsticky t))
                     (point)))
         (ov (make-overlay start end)))
    (overlay-put ov 'opencode-label t)
    (overlay-put ov extra-prop extra-value)
    (overlay-put ov 'display (propertize name 'face 'markdown-ts-code))
    (overlay-put ov 'modification-hooks '(opencode--remove-label))
    ov))


(defun opencode--add-file-to-context (file &optional mime display-name)
  "Add FILE to context with optional MIME and DISPLAY-NAME."
  (let* ((display-name (or display-name
                           (opencode--relative-path-for-display file)))
         (mime (or mime (opencode--mimetype file)))
         (url (concat "file://" file)))
    (push `((type . file)
            (filename . ,display-name)
            (mime . ,mime)
            (url . ,url))
          opencode--extra-parts)
    (opencode--insert-intangible display-name 'file-url url)
    (insert " ")))


(defun opencode--image-extension (mime)
  "Return a file extension for image MIME."
  (or (cadr (split-string mime "[/;+]" t))
      "img"))


(defun opencode--yank-image (type data)
  "Add pasted image DATA of MIME TYPE to the next prompt."
  (unless (comint-after-pmark-p)
    (user-error "Images can only be pasted into the current prompt"))
  (let* ((mime (symbol-name type))
         (extension (opencode--image-extension mime))
         (file (make-temp-file "opencode-image-" nil
                               (concat "." extension))))
    (condition-case error
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (write-region data nil file nil 'silent))
          (push file opencode--temp-files)
          (opencode--add-file-to-context file mime (file-name-nondirectory file)))
      (error
       (ignore-errors (delete-file file))
       (signal (car error) (cdr error))))))


(defun opencode--delete-temp-files (files)
  "Delete temporary FILES created by opencode."
  (dolist (file files)
    (when (and file (file-exists-p file))
      (ignore-errors (delete-file file)))))


(defun opencode--session-cleanup ()
  "Clean up resources for the current session buffer."
  (when-let* ((process (get-buffer-process (current-buffer))))
    (delete-process process))
  (opencode--delete-temp-files opencode--temp-files)
  (setq opencode--temp-files nil))


(defmacro with-last-opencode-session (&rest body)
  "Run BODY with the last used opencode session buffer active."
  `(if opencode-last-session-buffer
       (if (buffer-live-p opencode-last-session-buffer)
           (with-current-buffer opencode-last-session-buffer
             (unless (comint-after-pmark-p)
               (goto-char (point-max)))
             ,@body)
         (user-error "Last selected OpenCode session buffer is no longer active"))
     (user-error "Open an OpenCode session first")))


(defun opencode-add-file (&optional file prompt-directories)
  "Add a FILE to context, or prompt for file in current project.
With a prefix argument, show project directories instead of files."
  (interactive (list nil current-prefix-arg))
  (with-last-opencode-session
   (let* ((project (project-current t))
          (root (project-root project))
          (all-files (project-files project)))
     (opencode--add-file-to-context
      (or file
          (project--read-file-name
           project
           "Add to context"
           (if prompt-directories
               (delete root
                       (delete-dups
                        (cons
                         (concat root "./")
                         (mapcar #'file-name-directory all-files))))
             all-files)
           nil
           'file-name-history))))))


(defun opencode-add-file-dwim ()
  "If in Dired, add all marked files, or file at point if none marked.
Otherwise add the current buffer's file.
Otherwise prompt for file in current project."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (if-let* ((files (dired-get-marked-files)))
          (mapc #'opencode-add-file files)
        (if-let* ((file (dired-get-filename nil t)))
            (opencode-add-file file)
          (user-error "No file at point, and no marked files")))
    (opencode-add-file (buffer-file-name))))


(defun opencode-add-buffer-dwim ()
  "Add current buffer or prompt for one to add."
  (interactive)
  (let ((buffer (if opencode-session-id
                    (read-buffer "Add to context: ")
                  (buffer-name))))
    (with-last-opencode-session
     (push `((type . text)
             (text . ,(concat
                       (format "<buffer name=\"%s\">" buffer)
                       (with-current-buffer buffer
                         (buffer-substring-no-properties (point-min) (point-max)))
                       "</buffer>"))
             (synthetic . t)
             (metadata . ((buffer-name . ,buffer))))
           opencode--extra-parts)
     (opencode--insert-intangible buffer 'buffer-name buffer))))


(defun opencode-add-region ()
  "Add the active region to context."
  (interactive)
  (if (use-region-p)
      (let ((region (buffer-substring-no-properties (region-beginning)
                                                    (region-end)))
            (region-id (gensym)))
        (with-last-opencode-session
         (push `((type . text)
                 (text . ,(concat "<region>" region "</region>"))
                 (synthetic . t)
                 (metadata . ((region-id . ,region-id))))
               opencode--extra-parts)
         (opencode--insert-intangible
          (format "region: %s"
                  (truncate-string-to-width region 24 0 nil (truncate-string-ellipsis)))
          'region-id region-id)
         (insert " ")))
    (user-error "No active region")))


(defun opencode-add-subagent ()
  "Insert a subagent mention into the current input."
  (interactive)
  (call-interactively #'self-insert-command)
  (when (comint-after-pmark-p)
    (let ((subagents (cl-loop for agent in opencode-agents
                              when (string= "subagent" (alist-get 'mode agent))
                              collect agent)))
      (unless subagents
        (user-error "No subagents available"))
      (let ((name (opencode--annotated-completion
                   "Subagent: "
                   (cl-loop for agent in subagents
                            collect (let-alist agent
                                      (list .name .name .description))))))
        (push `((type . "agent")
                (name . ,name))
              opencode--extra-parts)
        (delete-char -1)
        (opencode--insert-intangible (concat "@" name) 'agent-name name)))))


(defun opencode-add-skill ()
  "Add an OpenCode V2 skill to the next prompt."
  (interactive)
  (unless (comint-after-pmark-p)
    (user-error "Skills can only be added in the current prompt"))
  (opencode-api-skills skills
    (let ((skill (opencode--annotated-completion
                  "Skill: "
                  (cl-loop for skill in skills
                           collect (list (or (alist-get 'name skill)
                                             (alist-get 'id skill))
                                         (alist-get 'id skill)
                                         (or (alist-get 'description skill) ""))))))
      (when skill
        (push `((type . "skill") (id . ,skill))
              opencode--extra-parts)
        (opencode--insert-intangible skill 'skill-id skill)))))

(defun opencode-toggle-prompt-delivery ()
  "Cycle the V2 inbox delivery mode for prompts."
  (interactive)
  (setq opencode-prompt-delivery
        (pcase opencode-prompt-delivery
          ("steer" "queue")
          ("queue" nil)
          (_ "steer")))
  (message "OpenCode prompt delivery: %s"
           (or opencode-prompt-delivery "server default")))

(defun opencode--prompt-payload (string parts)
  "Convert STRING and V1-style PARTS to a V2 prompt payload.
Return (TEXT FILES AGENTS SKILLS)."
  (let (texts files agents skills)
    (dolist (part (reverse parts))
      (pcase (format "%s" (alist-get 'type part))
        ("text"
         (when-let* ((text (alist-get 'text part)))
           (unless (string-empty-p text)
             (push text texts))))
        ("file"
         (when-let* ((uri (or (alist-get 'url part)
                             (alist-get 'uri part))))
           (push `((uri . ,uri)
                   (name . ,(or (alist-get 'filename part)
                                (alist-get 'name part)
                                uri)))
                 files)))
        ("agent"
         (when-let* ((name (alist-get 'name part)))
           (push `((name . ,name)) agents)))
        ("skill"
         (when-let* ((id (alist-get 'id part)))
           (push `((id . ,id)) skills)))))
    (list (string-join (append (nreverse texts) (list string)) "\n\n")
          (nreverse files)
          (nreverse agents)
          (nreverse skills))))


(defun opencode-insert-newline ()
  "Insert a newline in the OpenCode prompt without sending the message.
This mirrors the TUI's multi-line input behavior; RET still sends."
  (interactive)
  (unless (comint-after-pmark-p)
    (user-error "Not in the OpenCode prompt"))
  (insert "\n"))

(defun opencode--send-input (_proc string)
  "Send STRING as input to current opencode session."
  (opencode--highlight-input)
  (opencode--output "\n")
  (let ((extra-parts opencode--extra-parts)
        (temp-files opencode--temp-files)
        sent-message)
    (setf opencode--extra-parts nil
          opencode--temp-files nil)
    (pcase-let ((`(,text ,files ,agents ,skills)
                 (opencode--prompt-payload string extra-parts)))
      (cond
       ((string-prefix-p "/" string)
        (let ((space-pos (seq-position string ?\s)))
          (opencode-api-execute-command
              (opencode-session-id)
              (append `((name . ,(substring string 1 space-pos))
                        (text . ,(if space-pos
                                     (substring string (1+ space-pos))
                                   "")))
                      (when files `((files . ,files)))
                      (when agents `((agents . ,agents)))
                      (when skills `((skills . ,skills)))
                      (when opencode-prompt-delivery
                        `((delivery . ,opencode-prompt-delivery))))
              _response)))
       ((string-prefix-p "!" string)
        (opencode-api-execute-shell (opencode-session-id)
            `((command . ,(substring string 1)))
            _response))
       (t
        (setq sent-message t)
        (opencode-api-send-message
            (opencode-session-id)
            (append `((text . ,text))
                    (when files `((files . ,files)))
                    (when agents `((agents . ,agents)))
                    (when skills `((skills . ,skills)))
                    (when opencode-prompt-delivery
                      `((delivery . ,opencode-prompt-delivery))))
            _result
          (opencode--delete-temp-files temp-files)))))
    (unless sent-message
      (opencode--delete-temp-files temp-files))))


(defun opencode-session--send-synthetic-input (string)
  "Send STRING to the current opencode session.
Preserve any pending input context while sending STRING as a plain prompt."
  (let* ((process (get-buffer-process (current-buffer)))
         (pending-prompt (copy-marker (process-mark process) t))
         (original-point (copy-marker (point) t))
         (prompt-start (opencode--session-process-position))
         start
         end)
    (unwind-protect
        (progn
          (let ((inhibit-read-only t))
            (save-excursion
              (goto-char prompt-start)
              (setq start (point))
              (insert string)
              (setq end (point))))
          (set-marker (process-mark process) end)
          (let ((comint-last-input-start start)
                (comint-last-input-end end)
                (opencode--extra-parts nil)
                (opencode--temp-files nil))
            (opencode--send-input process string)
            (opencode--maybe-insert-block-spacing)))
      (set-marker (process-mark process) pending-prompt)
      (goto-char original-point)
      (set-marker pending-prompt nil)
      (set-marker original-point nil))))


(provide 'opencode-prompt)
;;; opencode-prompt.el ends here
