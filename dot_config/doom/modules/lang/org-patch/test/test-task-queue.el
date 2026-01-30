;;; test-task-queue.el --- Tests for task queue system -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple tests for the task queue movement functions.
;; Run with: emacs -batch -L .. -l buttercup -l dayone-task-queue -l test-task-queue.el -f buttercup-run

;;; Code:

(require 'buttercup)
(require 'dayone-task-queue)

;;; Test Helper

(defmacro with-org-test-buffer (contents &rest body)
  "Create temp org buffer with CONTENTS, execute BODY at first heading."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,contents)
     (goto-char (point-min))
     (org-next-visible-heading 1)
     ,@body))

;;; Tests

(describe "Queue tag helpers"
  (describe "+patch-dayone/clear-queue-tags"
    (it "removes all queue tags from a heading"
      (with-org-test-buffer "* TODO Task :@@frontburner:@@shelf:"
        (+patch-dayone/clear-queue-tags)
        (expect (member "@@frontburner" (org-get-tags nil t)) :to-be nil)
        (expect (member "@@shelf" (org-get-tags nil t)) :to-be nil))))

  (describe "+patch-dayone/set-queue-tag"
    (it "sets a queue tag and clears others"
      (with-org-test-buffer "* TODO Task :@@frontburner:"
        (+patch-dayone/set-queue-tag "@@icebox")
        (expect (member "@@icebox" (org-get-tags nil t)) :to-be-truthy)
        (expect (member "@@frontburner" (org-get-tags nil t)) :to-be nil)))

    (it "preserves non-queue tags"
      (with-org-test-buffer "* TODO Task :project:@@frontburner:"
        (+patch-dayone/set-queue-tag "@@shelf")
        (expect (member "@@shelf" (org-get-tags nil t)) :to-be-truthy)
        (expect (member "project" (org-get-tags nil t)) :to-be-truthy)))))

(describe "Movement functions"
  (describe "+patch-dayone/freeze"
    (it "clears TODO state"
      (with-org-test-buffer "* TODO Active task"
        (+patch-dayone/freeze)
        (expect (org-get-todo-state) :to-be nil)))

    (it "sets @@icebox tag"
      (with-org-test-buffer "* TODO Active task"
        (+patch-dayone/freeze)
        (expect (member "@@icebox" (org-get-tags nil t)) :to-be-truthy)))

    (it "removes OPENED property"
      (with-org-test-buffer "* TODO Active task
:PROPERTIES:
:OPENED: 2024-01-15
:END:"
        (+patch-dayone/freeze)
        (expect (org-entry-get nil "OPENED") :to-be nil)))

    (it "clears other queue tags"
      (with-org-test-buffer "* TODO Task :@@frontburner:"
        (+patch-dayone/freeze)
        (expect (member "@@frontburner" (org-get-tags nil t)) :to-be nil)
        (expect (member "@@icebox" (org-get-tags nil t)) :to-be-truthy))))

  (describe "+patch-dayone/shelve"
    (it "sets TODO state to TODO"
      (with-org-test-buffer "* TODO Frozen task"
        (+patch-dayone/shelve)
        (expect (org-get-todo-state) :to-equal "TODO")))

    (it "sets @@shelf tag"
      (with-org-test-buffer "* TODO Task"
        (+patch-dayone/shelve)
        (expect (member "@@shelf" (org-get-tags nil t)) :to-be-truthy)))

    (it "removes OPENED property"
      (with-org-test-buffer "* TODO Task
:PROPERTIES:
:OPENED: 2024-01-15
:END:"
        (+patch-dayone/shelve)
        (expect (org-entry-get nil "OPENED") :to-be nil))))

  (describe "+patch-dayone/send-to-backburner"
    (it "sets TODO state to TODO"
      (with-org-test-buffer "* Frozen task"
        (+patch-dayone/send-to-backburner)
        (expect (org-get-todo-state) :to-equal "TODO")))

    (it "sets @@backburner tag"
      (with-org-test-buffer "* TODO Task"
        (+patch-dayone/send-to-backburner)
        (expect (member "@@backburner" (org-get-tags nil t)) :to-be-truthy)))

    (it "sets OPENED property when not present"
      (with-org-test-buffer "* TODO Task"
        (+patch-dayone/send-to-backburner)
        (expect (org-entry-get nil "OPENED") :to-be-truthy)))

    (it "preserves existing OPENED property"
      (with-org-test-buffer "* TODO Task
:PROPERTIES:
:OPENED: 2020-01-01
:END:"
        (+patch-dayone/send-to-backburner)
        (expect (org-entry-get nil "OPENED") :to-equal "2020-01-01"))))

  (describe "+patch-dayone/send-to-frontburner"
    (it "sets TODO state to TODO"
      (with-org-test-buffer "* Frozen task"
        (+patch-dayone/send-to-frontburner)
        (expect (org-get-todo-state) :to-equal "TODO")))

    (it "sets @@frontburner tag"
      (with-org-test-buffer "* TODO Task"
        (+patch-dayone/send-to-frontburner)
        (expect (member "@@frontburner" (org-get-tags nil t)) :to-be-truthy)))

    (it "sets OPENED property when not present"
      (with-org-test-buffer "* TODO Task"
        (+patch-dayone/send-to-frontburner)
        (expect (org-entry-get nil "OPENED") :to-be-truthy)))

    (it "clears schedule"
      (with-org-test-buffer "* TODO Task
SCHEDULED: <2024-06-01>"
        (+patch-dayone/send-to-frontburner)
        (expect (org-get-scheduled-time nil) :to-be nil)))))

(describe "Queue transitions"
  (it "can move from icebox to frontburner"
    (with-org-test-buffer "* Frozen idea :@@icebox:"
      (+patch-dayone/send-to-frontburner)
      (expect (org-get-todo-state) :to-equal "TODO")
      (expect (member "@@frontburner" (org-get-tags nil t)) :to-be-truthy)
      (expect (member "@@icebox" (org-get-tags nil t)) :to-be nil)
      (expect (org-entry-get nil "OPENED") :to-be-truthy)))

  (it "can move from frontburner to icebox"
    (with-org-test-buffer "* TODO Hot task :@@frontburner:
:PROPERTIES:
:OPENED: 2024-01-15
:END:"
      (+patch-dayone/freeze)
      (expect (org-get-todo-state) :to-be nil)
      (expect (member "@@icebox" (org-get-tags nil t)) :to-be-truthy)
      (expect (member "@@frontburner" (org-get-tags nil t)) :to-be nil)
      (expect (org-entry-get nil "OPENED") :to-be nil)))

  (it "can cycle through all queue levels"
    (with-org-test-buffer "* TODO New task"
      (+patch-dayone/shelve)
      (expect (member "@@shelf" (org-get-tags nil t)) :to-be-truthy)

      (+patch-dayone/send-to-backburner)
      (expect (member "@@backburner" (org-get-tags nil t)) :to-be-truthy)
      (expect (member "@@shelf" (org-get-tags nil t)) :to-be nil)

      (+patch-dayone/send-to-frontburner)
      (expect (member "@@frontburner" (org-get-tags nil t)) :to-be-truthy)
      (expect (member "@@backburner" (org-get-tags nil t)) :to-be nil)

      (+patch-dayone/freeze)
      (expect (member "@@icebox" (org-get-tags nil t)) :to-be-truthy)
      (expect (member "@@frontburner" (org-get-tags nil t)) :to-be nil))))

(provide 'test-task-queue)
;;; test-task-queue.el ends here
