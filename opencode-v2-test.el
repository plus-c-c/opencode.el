;;; opencode-v2-test.el --- V2 normalization tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'opencode-common)
(require 'opencode-v2)

(ert-deftest opencode-v2-session-model-alias-test ()
  "V2 sessions expose both model id and modelID and a directory."
  (let* ((raw '((data
                  (id . "ses_1")
                  (title . "Test")
                  (location (directory . "/tmp"))
                  (model (id . "m") (providerID . "p") (variant . "high")))))
         (session (opencode-v2--session raw)))
    (should (equal (alist-get 'id session) "ses_1"))
    (should (equal (alist-get 'directory session) "/tmp"))
    (should (equal (alist-get 'modelID (alist-get 'model session)) "m"))
    (should (equal (opencode-v2--status '((type . "running"))) "busy"))))

(ert-deftest opencode-v2-providers-test ()
  "V2 flat models are grouped by provider and variants become an alist."
  (let* ((raw '((data
                  ((modelID . "a") (providerID . "p")
                   (id . "a") (name . "A") (variants))
                  ((modelID . "b") (providerID . "p")
                   (id . "b") (name . "B") (variants))
                  ((modelID . "c") (providerID . "q")
                   (id . "c") (name . "C")
                   (variants ((id . "high") (settings (x . 1))))))))
         (providers (alist-get 'providers (opencode-v2--providers raw))))
    (should (= (length providers) 2))
    (should (= (length (alist-get 'models (car providers))) 2))
    (should (equal (alist-get 'high
                              (alist-get 'variants
                                         (alist-get (intern "c")
                                                    (alist-get 'models
                                                               (cadr providers)))))
                   '((x . 1))))))

(ert-deftest opencode-v2-message-test ()
  "V2 message content becomes V1-shaped info/parts."
  (let* ((content (list '((type . "text") (text . "hi"))
                        '((type . "tool") (id . "call_1") (name . "shell")
                          (state (status . "completed")
                                 (input (command . "echo hi"))
                                 (content ((type . "text") (text . "hi")))))))
         (raw (list (cons 'id "msg_1")
                    (cons 'sessionID "ses_1")
                    (cons 'type "assistant")
                    (cons 'time '((created . 1)))
                    (cons 'tokens '((input . 2) (output . 3)
                                    (reasoning . 1)
                                    (cache (read . 4) (write . 0))))
                    (cons 'content content)))
         (message (opencode-v2--message raw))
         (info (alist-get 'info message))
         (parts (alist-get 'parts message)))
    (should (equal (alist-get 'role info) "assistant"))
    (should (equal (alist-get 'tool (cadr parts)) "shell"))
    (should (equal (alist-get 'output (alist-get 'state (cadr parts))) "hi"))
    (should (equal (opencode--token-total (alist-get 'tokens info)) 10))))

(ert-deftest opencode-v2-session-messages-reverse-test ()
  "Newest-first V2 pages are reversed for replay."
  (let* ((raw '((data ((id . "m2") (sessionID . "s") (type . "user") (text . "b") (time))
                       ((id . "m1") (sessionID . "s") (type . "user") (text . "a") (time)))))
         (messages (opencode-v2--session-messages raw)))
    (should (equal (mapcar (lambda (m) (alist-get 'id (alist-get 'info m))) messages)
                   '("m1" "m2")))))

(ert-deftest opencode-prompt-payload-test ()
  "Prompt payload carries text, files, agents and skills."
  (require 'opencode-prompt)
  (let ((payload (opencode--prompt-payload
                  "hello"
                  '(((type . "text") (text . "ctx"))
                    ((type . "file") (url . "file:///tmp/x") (filename . "x"))
                    ((type . "agent") (name . "explore"))
                    ((type . "skill") (id . "sk1"))))))
    (should (equal (nth 0 payload) "ctx\n\nhello"))
    (should (equal (alist-get 'name (car (nth 1 payload))) "x"))
    (should (equal (alist-get 'name (car (nth 2 payload))) "explore"))
    (should (equal (alist-get 'id (car (nth 3 payload))) "sk1"))))

(provide 'opencode-v2-test)
;;; opencode-v2-test.el ends here
