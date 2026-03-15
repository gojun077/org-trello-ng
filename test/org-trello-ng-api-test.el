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

(ert-deftest org-trello-ng-api-test-post-mock ()
  "Test POST wrapper with mocked HTTP call."
  (cl-letf (((symbol-function 'org-trello-ng-auth-get-credentials)
             (lambda () '(:key "fake-key" :token "fake-token")))
            ((symbol-function 'url-retrieve-synchronously)
             (lambda (_url &rest _args)
               (let ((buf (generate-new-buffer " *test-post*")))
                 (with-current-buffer buf
                   (insert "HTTP/1.1 200 OK\n")
                   (insert "Content-Type: application/json\n")
                   (insert "\n")
                   (insert "{\"id\":\"abc123\",\"name\":\"Test Card\"}"))
                 buf))))
    (let ((result (org-trello-ng-api-post "/cards"
                                          '((name . "Test Card")
                                            (idList . "list123")))))
      (should result)
      (should (equal (plist-get result :id) "abc123"))
      (should (equal (plist-get result :name) "Test Card")))))

(provide 'org-trello-ng-api-test)
;;; org-trello-ng-api-test.el ends here
