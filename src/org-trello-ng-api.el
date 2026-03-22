;;; org-trello-ng-api.el --- Trello REST API wrapper -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Jun Koh
;; Author: Peter Jun Koh <gopeterjun@naver.com>

;;; Commentary:
;; Wrapper functions for Trello REST API calls using url.el.

;;; Code:

(require 'url)
(require 'json)
(require 'seq)
(require 'org-trello-ng-auth)

(defconst org-trello-ng-api-base-url "https://api.trello.com/1"
  "Base URL for Trello API v1.")

(define-error 'org-trello-ng-api-error "Trello API error")
(define-error 'org-trello-ng-api-client-error "Trello API client error (4xx)"
  'org-trello-ng-api-error)
(define-error 'org-trello-ng-api-server-error "Trello API server error (5xx)"
  'org-trello-ng-api-error)

(defun org-trello-ng-api--build-url (endpoint &optional extra-params)
  "Build full API URL for ENDPOINT with auth and EXTRA-PARAMS."
  (let ((creds (org-trello-ng-auth-get-credentials)))
    (unless creds
      (error "Trello credentials not found in auth-source"))
    (let ((params (append (list (list "key" (plist-get creds :key))
                                (list "token" (plist-get creds :token)))
                          extra-params)))
      (concat org-trello-ng-api-base-url
              endpoint
              "?"
              (url-build-query-string params)))))

(defun org-trello-ng-api--parse-status (buffer)
  "Extract HTTP status code from BUFFER as an integer."
  (with-current-buffer buffer
    (goto-char (point-min))
    (if (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" (line-end-position) t)
        (string-to-number (match-string 1))
      0)))

(defun org-trello-ng-api--handle-response (buffer)
  "Parse response from BUFFER; signal an error for non-2xx status codes.
Returns the parsed JSON body as a Lisp object on success."
  (let ((status (org-trello-ng-api--parse-status buffer)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "^$" nil t)
      (let* ((json-object-type 'plist)
             (json-array-type 'list)
             (body (condition-case nil (json-read) (error nil))))
        (cond
         ((<= 200 status 299)
          body)
         ((<= 400 status 499)
          (signal 'org-trello-ng-api-client-error
                  (list :status status :body body)))
         ((<= 500 status 599)
          (signal 'org-trello-ng-api-server-error
                  (list :status status :body body)))
         (t
          (signal 'org-trello-ng-api-error
                  (list :status status :body body))))))))

(defun org-trello-ng-api-get (endpoint &optional params)
  "Make GET request to ENDPOINT with optional PARAMS.
Returns parsed JSON response."
  (let* ((url (org-trello-ng-api--build-url endpoint params))
         (buffer (url-retrieve-synchronously url t t 30)))
    (unwind-protect
        (org-trello-ng-api--handle-response buffer)
      (kill-buffer buffer))))

(defun org-trello-ng-api-post (endpoint body &optional params)
  "Make POST request to ENDPOINT with JSON BODY and optional PARAMS.
BODY is an alist that will be JSON-encoded.  Returns parsed JSON response."
  (let* ((url (org-trello-ng-api--build-url endpoint params))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string (json-encode body) 'utf-8))
         (buffer (url-retrieve-synchronously url t t 30)))
    (unwind-protect
        (org-trello-ng-api--handle-response buffer)
      (kill-buffer buffer))))

(defun org-trello-ng-api-delete (endpoint &optional params)
  "Make DELETE request to ENDPOINT with optional PARAMS.
Returns parsed JSON response."
  (let* ((url (org-trello-ng-api--build-url endpoint params))
         (url-request-method "DELETE")
         (buffer (url-retrieve-synchronously url t t 30)))
    (unwind-protect
        (org-trello-ng-api--handle-response buffer)
      (kill-buffer buffer))))

(defconst org-trello-ng-api-batch-max 10
  "Maximum number of URLs per Trello batch request.")

(defun org-trello-ng-api-batch-get (endpoints)
  "Batch multiple GET ENDPOINTS into a single HTTP call via /1/batch.
ENDPOINTS is a list of API path strings (e.g., \"/boards/ID/cards\").
Trello limits batch requests to 10 URLs per call; split larger
lists into chunks automatically.
Return a flat list of response objects, one per endpoint."
  (let ((results '()))
    (while endpoints
      (let* ((chunk (seq-take endpoints org-trello-ng-api-batch-max))
             (rest (seq-drop endpoints org-trello-ng-api-batch-max))
             (urls-param (mapconcat #'identity chunk ","))
             (response (org-trello-ng-api-get "/batch"
                                              (list (list "urls" urls-param)))))
        (setq results (append results response))
        (setq endpoints rest)))
    results))

(defun org-trello-ng-api-get-my-boards ()
  "Fetch all boards for the authenticated user."
  (org-trello-ng-api-get "/members/me/boards"))

(provide 'org-trello-ng-api)
;;; org-trello-ng-api.el ends here
