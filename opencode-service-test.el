;;; opencode-service-test.el --- Service discovery tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for choosing the OpenCode V2 background service over the
;; legacy fixed-port server.

;;; Code:

(require 'ert)
(require 'opencode)

(ert-deftest opencode-autoconnect-starts-shared-service ()
  "When no registered service exists, start the shared service."
  (let ((opencode--event-subscription nil)
        (opencode--process nil)
        (opencode--service-start-process nil)
        started)
    (cl-letf (((symbol-function 'opencode--service-info)
               (lambda () nil))
              ((symbol-function 'opencode--server-running-p)
               (lambda () nil))
              ((symbol-function 'opencode--start-service)
               (lambda (_on-connect)
                 (setq started t))))
      (opencode-autoconnect #'ignore))
    (should started)))

(ert-deftest opencode-connect-to-service-uses-registered-password ()
  "Connect to the service URL with the password from service.json."
  (let ((opencode--event-subscription nil)
        captured-url captured-password)
    (cl-letf (((symbol-function 'opencode--service-info)
               (lambda ()
                 '((url . "http://127.0.0.1:49374")
                   (password . "service-password"))))
              ((symbol-function 'opencode-connect-url)
               (lambda (url on-connect password)
                 (setq captured-url url
                       captured-password password)
                 (funcall on-connect))))
      (opencode--connect-to-service #'ignore))
    (should (equal captured-url "http://127.0.0.1:49374"))
    (should (equal captured-password "service-password"))))

(provide 'opencode-service-test)
;;; opencode-service-test.el ends here
