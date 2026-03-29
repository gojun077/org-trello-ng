;;; org-trello-ng-buffer.el --- Org-mode buffer model for org-trello-ng -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Peter Jun Koh
;; Author: Peter Jun Koh <gopeterjun@naver.com>

;;; Commentary:
;; Defines the data model mapping between org-mode buffers and Trello
;; boards.  An org file linked to a Trello board stores board metadata
;; in a file-level :PROPERTIES: drawer at the top of the buffer.
;;
;; File-level :PROPERTIES: drawer schema
;; ======================================
;; The very first element in a linked org buffer is a top-level
;; heading (level 0 or "before first heading") property drawer that
;; holds board-wide metadata:
;;
;;   #+PROPERTY: board-id      <Trello board UUID>
;;   #+PROPERTY: board-name    <human-readable board name>
;;   #+PROPERTY: trello-user-id <Trello member UUID>
;;   #+PROPERTY: list-TODO     <Trello list UUID for TODO column>
;;   #+PROPERTY: list-Anywhere <Trello list UUID for Anywhere column>
;;   #+PROPERTY: list-DONE     <Trello list UUID for DONE column>
;;   #+PROPERTY: list-Canceled <Trello list UUID for Canceled column>
;;
;; List properties follow the naming convention "list-<KEYWORD>" where
;; <KEYWORD> matches an `org-todo-keywords' keyword or Trello list name.
;; The set of list-* properties varies per board.
;;
;; These are stored as org buffer-wide properties using #+PROPERTY:
;; keywords, making them accessible via `org-entry-get' with the
;; special point-or-marker argument nil (file-level) and readable
;; without expanding drawers.

;;; Code:

(defconst org-trello-ng-buffer-board-id-prop "board-id"
  "Property name for the Trello board UUID.")

(defconst org-trello-ng-buffer-board-name-prop "board-name"
  "Property name for the Trello board display name.")

(defconst org-trello-ng-buffer-user-id-prop "trello-user-id"
  "Property name for the authenticated Trello member UUID.")

(defconst org-trello-ng-buffer-list-prefix "list-"
  "Prefix for list UUID properties in the file-level drawer.
Each Trello list is stored as a property named \"list-<KEYWORD>\"
where KEYWORD is the org TODO keyword or Trello list name.
For example: list-TODO, list-Anywhere, list-DONE, list-Canceled.")

(defconst org-trello-ng-buffer-file-props
  (list org-trello-ng-buffer-board-id-prop
        org-trello-ng-buffer-board-name-prop
        org-trello-ng-buffer-user-id-prop)
  "Required file-level property names (excluding dynamic list-* entries).")

(provide 'org-trello-ng-buffer)
;;; org-trello-ng-buffer.el ends here
