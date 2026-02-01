;;; org-trello-ng-auth.el --- Authentication for org-trello-ng -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Jun Koh
;; Author: Peter Jun Koh <gopeterjun@naver.com>

;;; Commentary:
;; Retrieve Trello API credentials from auth-source (e.g., ~/.authinfo.gpg).
;; Expected entry format:
;;   machine api.trello.com login <api-key> password <token>

;;; Code:

(require 'auth-source)

(defconst org-trello-ng-auth-host "api.trello.com"
  "Host used for auth-source lookup.")

(defun org-trello-ng-auth-get-credentials ()
  "Retrieve Trello API key and token from auth-source.
Returns a plist (:key KEY :token TOKEN) or nil if not found."
  (let ((found (car (auth-source-search :host org-trello-ng-auth-host
                                        :max 1
                                        :require '(:user :secret)))))
    (when found
      (let ((key (plist-get found :user))
            (secret (plist-get found :secret)))
        (list :key key
              :token (if (functionp secret)
                         (funcall secret)
                       secret))))))

(provide 'org-trello-ng-auth)
;;; org-trello-ng-auth.el ends here
