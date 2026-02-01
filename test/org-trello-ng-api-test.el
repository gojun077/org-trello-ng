;;; org-trello-ng-api-test.el --- Tests for org-trello-ng-api -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for Trello API module.
;; These tests make real API calls and require valid credentials.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "../src" (file-name-directory load-file-name)))
(require 'org-trello-ng-api)

(ert-deftest org-trello-ng-api-test-get-my-boards ()
  "Test fetching boards for authenticated user."
  (let ((boards (org-trello-ng-api-get-my-boards)))
    (should boards)
    (should (listp boards))
    (when boards
      (let ((first-board (car boards)))
        (should (plist-get first-board :id))
        (should (plist-get first-board :name))))))

(provide 'org-trello-ng-api-test)
;;; org-trello-ng-api-test.el ends here
