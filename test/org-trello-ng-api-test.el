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

(ert-deftest org-trello-ng-api-test-put-mock ()
  "Test PUT wrapper with mocked HTTP call."
  (cl-letf (((symbol-function 'org-trello-ng-auth-get-credentials)
             (lambda () '(:key "fake-key" :token "fake-token")))
            ((symbol-function 'url-retrieve-synchronously)
             (lambda (_url &rest _args)
               (let ((buf (generate-new-buffer " *test-put*")))
                 (with-current-buffer buf
                   (insert "HTTP/1.1 200 OK\n")
                   (insert "Content-Type: application/json\n")
                   (insert "\n")
                   (insert "{\"id\":\"card1\",\"name\":\"Updated Card\"}"))
                 buf))))
    (let ((result (org-trello-ng-api-put "/cards/card1"
                                          '((name . "Updated Card")))))
      (should result)
      (should (equal (plist-get result :id) "card1"))
      (should (equal (plist-get result :name) "Updated Card")))))

(ert-deftest org-trello-ng-api-test-put-sends-put-method ()
  "Test that PUT wrapper uses PUT HTTP method."
  (let ((captured-method nil))
    (cl-letf (((symbol-function 'org-trello-ng-auth-get-credentials)
               (lambda () '(:key "fake-key" :token "fake-token")))
              ((symbol-function 'url-retrieve-synchronously)
               (lambda (_url &rest _args)
                 (setq captured-method url-request-method)
                 (let ((buf (generate-new-buffer " *test-put-method*")))
                   (with-current-buffer buf
                     (insert "HTTP/1.1 200 OK\n")
                     (insert "Content-Type: application/json\n")
                     (insert "\n")
                     (insert "{\"id\":\"x\"}"))
                   buf))))
      (org-trello-ng-api-put "/cards/x" '((name . "X")))
      (should (equal captured-method "PUT")))))

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
  "Test handle-response signals rate-limit error for 429."
  (let ((buf (generate-new-buffer " *test-429*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "HTTP/1.1 429 Too Many Requests\n\n")
            (insert "\"Rate limit exceeded\""))
          (let ((err (should-error
                      (org-trello-ng-api--handle-response buf)
                      :type 'org-trello-ng-api-rate-limit-error)))
            (should (= (plist-get (cdr err) :status) 429))))
      (kill-buffer buf))))

(ert-deftest org-trello-ng-api-test-handle-response-429-is-client-error ()
  "Test that 429 rate-limit error is also a client error (inheritance)."
  (let ((buf (generate-new-buffer " *test-429-inherit*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "HTTP/1.1 429 Too Many Requests\n\n")
            (insert "\"Rate limit exceeded\""))
          (should-error (org-trello-ng-api--handle-response buf)
                        :type 'org-trello-ng-api-client-error))
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

;;; Rate-limit retry tests

(ert-deftest org-trello-ng-api-test-retry-succeeds-after-429 ()
  "Test that retry wrapper retries on 429 and succeeds."
  (let ((call-count 0))
    (cl-letf (((symbol-function 'sleep-for) (lambda (_s))))
      (let ((org-trello-ng-api-max-retries 3)
            (org-trello-ng-api-retry-base-delay 0.0))
        (let ((result (org-trello-ng-api--with-retry
                       (lambda ()
                         (setq call-count (1+ call-count))
                         (when (< call-count 3)
                           (signal 'org-trello-ng-api-rate-limit-error
                                   (list :status 429 :body nil)))
                         '(:ok t)))))
          (should (equal result '(:ok t)))
          (should (= call-count 3)))))))

(ert-deftest org-trello-ng-api-test-retry-exhausted ()
  "Test that retry wrapper signals error after max retries."
  (cl-letf (((symbol-function 'sleep-for) (lambda (_s))))
    (let ((org-trello-ng-api-max-retries 2)
          (org-trello-ng-api-retry-base-delay 0.0)
          (call-count 0))
      (should-error
       (org-trello-ng-api--with-retry
        (lambda ()
          (setq call-count (1+ call-count))
          (signal 'org-trello-ng-api-rate-limit-error
                  (list :status 429 :body nil))))
       :type 'org-trello-ng-api-rate-limit-error)
      (should (= call-count 3)))))

(ert-deftest org-trello-ng-api-test-retry-no-retry-on-other-errors ()
  "Test that retry wrapper does not retry on non-429 errors."
  (let ((call-count 0))
    (should-error
     (org-trello-ng-api--with-retry
      (lambda ()
        (setq call-count (1+ call-count))
        (signal 'org-trello-ng-api-client-error
                (list :status 400 :body nil))))
     :type 'org-trello-ng-api-client-error)
    (should (= call-count 1))))

(ert-deftest org-trello-ng-api-test-retry-exponential-delays ()
  "Test that retry uses exponential backoff delays."
  (let ((delays '())
        (call-count 0))
    (cl-letf (((symbol-function 'sleep-for)
               (lambda (s) (push s delays))))
      (let ((org-trello-ng-api-max-retries 3)
            (org-trello-ng-api-retry-base-delay 1.0)
            (org-trello-ng-api-retry-max-delay 30.0))
        (org-trello-ng-api--with-retry
         (lambda ()
           (setq call-count (1+ call-count))
           (when (< call-count 4)
             (signal 'org-trello-ng-api-rate-limit-error
                     (list :status 429 :body nil)))
           :done))
        (setq delays (nreverse delays))
        (should (= (length delays) 3))
        (should (= (nth 0 delays) 1.0))
        (should (= (nth 1 delays) 2.0))
        (should (= (nth 2 delays) 4.0))))))

(ert-deftest org-trello-ng-api-test-retry-max-delay-cap ()
  "Test that retry delay is capped at max-delay."
  (let ((delays '())
        (call-count 0))
    (cl-letf (((symbol-function 'sleep-for)
               (lambda (s) (push s delays))))
      (let ((org-trello-ng-api-max-retries 5)
            (org-trello-ng-api-retry-base-delay 1.0)
            (org-trello-ng-api-retry-max-delay 3.0))
        (org-trello-ng-api--with-retry
         (lambda ()
           (setq call-count (1+ call-count))
           (when (< call-count 5)
             (signal 'org-trello-ng-api-rate-limit-error
                     (list :status 429 :body nil)))
           :done))
        (setq delays (nreverse delays))
        (should (= (nth 0 delays) 1.0))
        (should (= (nth 1 delays) 2.0))
        (should (= (nth 2 delays) 3.0))
        (should (= (nth 3 delays) 3.0))))))

(ert-deftest org-trello-ng-api-test-get-retries-on-429 ()
  "Test that api-get retries on 429 via the retry wrapper."
  (let ((call-count 0))
    (cl-letf (((symbol-function 'org-trello-ng-auth-get-credentials)
               (lambda () '(:key "fake-key" :token "fake-token")))
              ((symbol-function 'url-retrieve-synchronously)
               (lambda (_url &rest _args)
                 (setq call-count (1+ call-count))
                 (let ((buf (generate-new-buffer " *test-retry-get*")))
                   (with-current-buffer buf
                     (if (< call-count 2)
                         (progn
                           (insert "HTTP/1.1 429 Too Many Requests\n\n")
                           (insert "\"Rate limit exceeded\""))
                       (insert "HTTP/1.1 200 OK\n")
                       (insert "Content-Type: application/json\n\n")
                       (insert "{\"id\":\"card1\"}")))
                   buf)))
              ((symbol-function 'sleep-for) (lambda (_s))))
      (let ((org-trello-ng-api-max-retries 3)
            (org-trello-ng-api-retry-base-delay 0.0))
        (let ((result (org-trello-ng-api-get "/cards/card1")))
          (should (equal (plist-get result :id) "card1"))
          (should (= call-count 2)))))))

(provide 'org-trello-ng-api-test)
;;; org-trello-ng-api-test.el ends here
