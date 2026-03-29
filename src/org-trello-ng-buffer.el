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
;;
;; Per-heading :PROPERTIES: drawer schema
;; ======================================
;; Each org heading representing a Trello card carries its own
;; :PROPERTIES: drawer with the following fields:
;;
;;   :PROPERTIES:
;;   :card-id:  <Trello card UUID>
;;   :board-id: <Trello board UUID>
;;   :sha1:     <SHA-1 hash of the heading text + body>
;;   :card-pos: <numeric position on the Trello list>
;;   :END:
;;
;; - card-id   — the Trello card UUID; set after first sync.
;; - board-id  — the Trello board UUID this card belongs to.
;; - sha1      — SHA-1 hash of the org heading text and body,
;;               used to skip syncing tasks that have not changed.
;; - card-pos  — optional; numeric position of the card in its
;;               Trello list, used to preserve ordering.
;;
;; These properties are local-only metadata; they are never synced
;; to Trello.  They are read/written via `org-entry-get' and
;; `org-entry-put' at heading level.

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

;;; Per-heading property constants

(defconst org-trello-ng-buffer-card-id-prop "card-id"
  "Property name for the Trello card UUID on a heading.")

(defconst org-trello-ng-buffer-heading-board-id-prop "board-id"
  "Property name for the Trello board UUID on a heading.")

(defconst org-trello-ng-buffer-sha1-prop "sha1"
  "Property name for the SHA-1 hash of the heading content.
Used to detect whether an org task has changed since the last sync.")

(defconst org-trello-ng-buffer-card-pos-prop "card-pos"
  "Property name for the card position on its Trello list.
Optional; used to preserve card ordering.")

(defconst org-trello-ng-buffer-heading-props
  (list org-trello-ng-buffer-card-id-prop
        org-trello-ng-buffer-heading-board-id-prop
        org-trello-ng-buffer-sha1-prop
        org-trello-ng-buffer-card-pos-prop)
  "All per-heading property names for Trello card metadata.")

(defconst org-trello-ng-buffer-heading-required-props
  (list org-trello-ng-buffer-card-id-prop
        org-trello-ng-buffer-heading-board-id-prop
        org-trello-ng-buffer-sha1-prop)
  "Required per-heading property names (card-pos is optional).")

(provide 'org-trello-ng-buffer)
;;; org-trello-ng-buffer.el ends here
