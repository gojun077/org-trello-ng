;;; org-trello-ng-buffer-test.el --- Tests for org-trello-ng-buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the buffer data model module.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "../src" (file-name-directory load-file-name)))
(require 'org-trello-ng-buffer)

(ert-deftest org-trello-ng-buffer-test-board-id-prop ()
  "Test board-id property name constant."
  (should (equal org-trello-ng-buffer-board-id-prop "board-id")))

(ert-deftest org-trello-ng-buffer-test-board-name-prop ()
  "Test board-name property name constant."
  (should (equal org-trello-ng-buffer-board-name-prop "board-name")))

(ert-deftest org-trello-ng-buffer-test-user-id-prop ()
  "Test trello-user-id property name constant."
  (should (equal org-trello-ng-buffer-user-id-prop "trello-user-id")))

(ert-deftest org-trello-ng-buffer-test-list-prefix ()
  "Test list property prefix constant."
  (should (equal org-trello-ng-buffer-list-prefix "list-")))

(ert-deftest org-trello-ng-buffer-test-file-props-contains-required ()
  "Test that file-props list contains all required properties."
  (should (member "board-id" org-trello-ng-buffer-file-props))
  (should (member "board-name" org-trello-ng-buffer-file-props))
  (should (member "trello-user-id" org-trello-ng-buffer-file-props)))

(ert-deftest org-trello-ng-buffer-test-list-prop-naming ()
  "Test that list property names follow the list-<KEYWORD> convention."
  (let ((todo-prop (concat org-trello-ng-buffer-list-prefix "TODO"))
        (done-prop (concat org-trello-ng-buffer-list-prefix "DONE")))
    (should (equal todo-prop "list-TODO"))
    (should (equal done-prop "list-DONE"))))

;;; Per-heading property tests

(ert-deftest org-trello-ng-buffer-test-card-id-prop ()
  "Test card-id property name constant."
  (should (equal org-trello-ng-buffer-card-id-prop "card-id")))

(ert-deftest org-trello-ng-buffer-test-heading-board-id-prop ()
  "Test heading-level board-id property name constant."
  (should (equal org-trello-ng-buffer-heading-board-id-prop "board-id")))

(ert-deftest org-trello-ng-buffer-test-sha1-prop ()
  "Test sha1 property name constant."
  (should (equal org-trello-ng-buffer-sha1-prop "sha1")))

(ert-deftest org-trello-ng-buffer-test-card-pos-prop ()
  "Test card-pos property name constant."
  (should (equal org-trello-ng-buffer-card-pos-prop "card-pos")))

(ert-deftest org-trello-ng-buffer-test-heading-props-contains-all ()
  "Test that heading-props list contains all heading properties."
  (should (member "card-id" org-trello-ng-buffer-heading-props))
  (should (member "board-id" org-trello-ng-buffer-heading-props))
  (should (member "sha1" org-trello-ng-buffer-heading-props))
  (should (member "card-pos" org-trello-ng-buffer-heading-props))
  (should (= (length org-trello-ng-buffer-heading-props) 4)))

(ert-deftest org-trello-ng-buffer-test-heading-required-props ()
  "Test that required heading props exclude card-pos."
  (should (member "card-id" org-trello-ng-buffer-heading-required-props))
  (should (member "board-id" org-trello-ng-buffer-heading-required-props))
  (should (member "sha1" org-trello-ng-buffer-heading-required-props))
  (should-not (member "card-pos" org-trello-ng-buffer-heading-required-props))
  (should (= (length org-trello-ng-buffer-heading-required-props) 3)))

(ert-deftest org-trello-ng-buffer-test-heading-props-are-strings ()
  "Test that all heading property names are strings."
  (dolist (prop org-trello-ng-buffer-heading-props)
    (should (stringp prop))))

(provide 'org-trello-ng-buffer-test)
;;; org-trello-ng-buffer-test.el ends here
