;;; org-trello-ng-auth-test.el --- Tests for org-trello-ng-auth -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for authentication module.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "../src" (file-name-directory load-file-name)))
(require 'org-trello-ng-auth)

(ert-deftest org-trello-ng-auth-test-get-credentials ()
  "Test that credentials can be retrieved from auth-source."
  (let ((creds (org-trello-ng-auth-get-credentials)))
    (should creds)
    (should (plist-get creds :key))
    (should (plist-get creds :token))
    (should (stringp (plist-get creds :key)))
    (should (stringp (plist-get creds :token)))))

(provide 'org-trello-ng-auth-test)
;;; org-trello-ng-auth-test.el ends here
