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

(ert-deftest org-trello-ng-api-test-delete-mock ()
  "Test DELETE wrapper with mocked HTTP call."
  (cl-letf (((symbol-function 'org-trello-ng-auth-get-credentials)
             (lambda () '(:key "fake-key" :token "fake-token")))
            ((symbol-function 'url-retrieve-synchronously)
             (lambda (_url &rest _args)
               (let ((buf (generate-new-buffer " *test-delete*")))
                 (with-current-buffer buf
                   (insert "HTTP/1.1 200 OK\n")
                   (insert "Content-Type: application/json\n")
                   (insert "\n")
                   (insert "{\"_value\":null}"))
                 buf))))
    (let ((result (org-trello-ng-api-delete "/cards/abc123")))
      (should result))))

(ert-deftest org-trello-ng-api-test-delete-error ()
  "Test DELETE wrapper signals error for 404."
  (cl-letf (((symbol-function 'org-trello-ng-auth-get-credentials)
             (lambda () '(:key "fake-key" :token "fake-token")))
            ((symbol-function 'url-retrieve-synchronously)
             (lambda (_url &rest _args)
               (let ((buf (generate-new-buffer " *test-delete-err*")))
                 (with-current-buffer buf
                   (insert "HTTP/1.1 404 Not Found\n\n")
                   (insert "{\"message\":\"card not found\"}"))
                 buf))))
    (let ((err (should-error (org-trello-ng-api-delete "/cards/nonexistent")
                             :type 'org-trello-ng-api-client-error)))
      (should (= (plist-get (cdr err) :status) 404)))))

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

;;; HTTP status code handling tests

(ert-deftest org-trello-ng-api-test-parse-status-200 ()
  "Test parsing a 200 status code from response buffer."
  (let ((buf (generate-new-buffer " *test-status*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "HTTP/1.1 200 OK\nContent-Type: application/json\n\n{}"))
          (should (= (org-trello-ng-api--parse-status buf) 200)))
      (kill-buffer buf))))

(ert-deftest org-trello-ng-api-test-parse-status-404 ()
  "Test parsing a 404 status code from response buffer."
  (let ((buf (generate-new-buffer " *test-status*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "HTTP/1.1 404 Not Found\n\n"))
          (should (= (org-trello-ng-api--parse-status buf) 404)))
      (kill-buffer buf))))

(ert-deftest org-trello-ng-api-test-parse-status-missing ()
  "Test parsing when no HTTP status line is present."
  (let ((buf (generate-new-buffer " *test-status*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "garbage data\n\n"))
          (should (= (org-trello-ng-api--parse-status buf) 0)))
      (kill-buffer buf))))

(ert-deftest org-trello-ng-api-test-handle-response-200 ()
  "Test handle-response returns body for 200 status."
  (let ((buf (generate-new-buffer " *test-200*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "HTTP/1.1 200 OK\nContent-Type: application/json\n\n")
            (insert "{\"id\":\"card1\",\"name\":\"Test\"}"))
          (let ((result (org-trello-ng-api--handle-response buf)))
            (should result)
            (should (equal (plist-get result :id) "card1"))))
      (kill-buffer buf))))

(ert-deftest org-trello-ng-api-test-handle-response-401 ()
  "Test handle-response signals client error for 401."
  (let ((buf (generate-new-buffer " *test-401*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "HTTP/1.1 401 Unauthorized\n\n")
            (insert "\"unauthorized permission requested\""))
          (let ((err (should-error
                      (org-trello-ng-api--handle-response buf)
                      :type 'org-trello-ng-api-client-error)))
            (should (= (plist-get (cdr err) :status) 401))))
      (kill-buffer buf))))

(ert-deftest org-trello-ng-api-test-handle-response-404 ()
  "Test handle-response signals client error for 404."
  (let ((buf (generate-new-buffer " *test-404*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "HTTP/1.1 404 Not Found\n\n")
            (insert "{\"message\":\"not found\"}"))
          (let ((err (should-error
                      (org-trello-ng-api--handle-response buf)
                      :type 'org-trello-ng-api-client-error)))
            (should (= (plist-get (cdr err) :status) 404))
            (should (equal (plist-get (plist-get (cdr err) :body) :message)
                           "not found"))))
      (kill-buffer buf))))

(ert-deftest org-trello-ng-api-test-handle-response-500 ()
  "Test handle-response signals server error for 500."
  (let ((buf (generate-new-buffer " *test-500*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "HTTP/1.1 500 Internal Server Error\n\n")
            (insert "{\"error\":\"server failure\"}"))
          (let ((err (should-error
                      (org-trello-ng-api--handle-response buf)
                      :type 'org-trello-ng-api-server-error)))
            (should (= (plist-get (cdr err) :status) 500))))
      (kill-buffer buf))))

(ert-deftest org-trello-ng-api-test-handle-response-429 ()
  "Test handle-response signals client error for 429 rate limit."
  (let ((buf (generate-new-buffer " *test-429*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "HTTP/1.1 429 Too Many Requests\n\n")
            (insert "\"Rate limit exceeded\""))
          (let ((err (should-error
                      (org-trello-ng-api--handle-response buf)
                      :type 'org-trello-ng-api-client-error)))
            (should (= (plist-get (cdr err) :status) 429))))
      (kill-buffer buf))))

(ert-deftest org-trello-ng-api-test-get-signals-on-error ()
  "Test that api-get propagates HTTP errors."
  (cl-letf (((symbol-function 'org-trello-ng-auth-get-credentials)
             (lambda () '(:key "fake-key" :token "fake-token")))
            ((symbol-function 'url-retrieve-synchronously)
             (lambda (_url &rest _args)
               (let ((buf (generate-new-buffer " *test-get-err*")))
                 (with-current-buffer buf
                   (insert "HTTP/1.1 403 Forbidden\n\n")
                   (insert "\"invalid token\""))
                 buf))))
    (should-error (org-trello-ng-api-get "/boards/invalid")
                  :type 'org-trello-ng-api-client-error)))

(provide 'org-trello-ng-api-test)
;;; org-trello-ng-api-test.el ends here
