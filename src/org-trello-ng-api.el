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

(defun org-trello-ng-api--handle-response (buffer)
  "Parse JSON response from BUFFER and return as Lisp object."
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "^$" nil t)
    (let ((json-object-type 'plist)
          (json-array-type 'list))
      (json-read))))

(defun org-trello-ng-api-get (endpoint &optional params)
  "Make GET request to ENDPOINT with optional PARAMS.
Returns parsed JSON response."
  (let* ((url (org-trello-ng-api--build-url endpoint params))
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
