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

(ert-deftest org-trello-ng-api-test-batch-get-mock ()
  "Test batch GET with mocked HTTP call."
  (cl-letf (((symbol-function 'org-trello-ng-auth-get-credentials)
             (lambda () '(:key "fake-key" :token "fake-token")))
            ((symbol-function 'url-retrieve-synchronously)
             (lambda (_url &rest _args)
               (let ((buf (generate-new-buffer " *test-batch*")))
                 (with-current-buffer buf
                   (insert "HTTP/1.1 200 OK\n")
                   (insert "Content-Type: application/json\n")
                   (insert "\n")
                   (insert "[{\"200\":{\"id\":\"b1\",\"name\":\"Board1\"}}")
                   (insert ",{\"200\":{\"id\":\"b2\",\"name\":\"Board2\"}}]"))
                 buf))))
    (let ((result (org-trello-ng-api-batch-get
                   '("/boards/b1" "/boards/b2"))))
      (should result)
      (should (= (length result) 2)))))

(ert-deftest org-trello-ng-api-test-batch-get-chunking ()
  "Test that batch GET splits lists larger than 10 into chunks."
  (let ((call-count 0))
    (cl-letf (((symbol-function 'org-trello-ng-auth-get-credentials)
               (lambda () '(:key "fake-key" :token "fake-token")))
              ((symbol-function 'url-retrieve-synchronously)
               (lambda (_url &rest _args)
                 (setq call-count (1+ call-count))
                 (let ((buf (generate-new-buffer " *test-batch-chunk*")))
                   (with-current-buffer buf
                     (insert "HTTP/1.1 200 OK\n")
                     (insert "Content-Type: application/json\n")
                     (insert "\n")
                     (insert "[{\"200\":{\"id\":\"x\"}}]"))
                   buf))))
      (let* ((endpoints (make-list 12 "/boards/x"))
             (result (org-trello-ng-api-batch-get endpoints)))
        (should (= call-count 2))
        (should (= (length result) 2))))))

(ert-deftest org-trello-ng-api-test-batch-get-empty ()
  "Test batch GET with empty endpoint list."
  (let ((result (org-trello-ng-api-batch-get '())))
    (should (null result))))

(provide 'org-trello-ng-api-test)
;;; org-trello-ng-api-test.el ends here
