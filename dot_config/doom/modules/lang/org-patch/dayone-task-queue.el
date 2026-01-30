;;; dayone-task-queue.el --- Task queue system for GTD workflow -*- lexical-binding: t; -*-
;;; Commentary:
;; Core functions for managing task priority queues.
;; Queues from coldest to hottest: icebox -> shelf -> backburner -> frontburner
;;; Code:
(require 'org)
(require 'org-agenda)
(require 'ts)

(defconst +patch-dayone/queue-tags
  '("@@icebox" "@@shelf" "@@backburner" "@@frontburner")
  "All queue tags, in order from coldest to hottest.")

(defun +patch-dayone/clear-queue-tags ()
  "Remove all queue tags from the current heading."
  (dolist (tag +patch-dayone/queue-tags)
    (org-toggle-tag tag 'off)))

(defun +patch-dayone/set-queue-tag (tag)
  "Set TAG as the queue tag, removing any other queue tags."
  (+patch-dayone/clear-queue-tags)
  (org-toggle-tag tag 'on))

(defun +patch/set-opened-date (&optional pom date)
  "Set the OPENED date of a task at POM (default point).
DATE defaults to today if not specified."
  (let* ((pom (or pom (point)))
         (date (or date (ts-format (ts-now))))
         (date-str (ts-format "%Y-%m-%d" (ts-parse date))))
    (org-entry-put pom "OPENED" date-str)))

(defun +patch-dayone/clean-task ()
  "Clean task metadata when demoting to icebox or shelf.
Removes priority, schedule, OPENED, and planning properties."
  (ignore-errors (org-priority 'remove))
  (ignore-errors (org-schedule '(4)))
  (ignore-errors (org-entry-delete (point) "OPENED"))
  (ignore-errors (org-entry-delete (point) "PLANNED-FOR-QUARTER"))
  (ignore-errors (org-entry-delete (point) "PLANNED-FOR-YEAR"))
  (+patch-dayone/clear-queue-tags))

(defun +patch-dayone/freeze (&optional pom)
  "Move task to icebox (someday/maybe).
Clears scheduling, priority, OPENED, and planning properties."
  (interactive)
  (save-excursion
    (when pom (goto-char pom))
    (+patch-dayone/clean-task)
    (org-todo "")  ; clear TODO state
    (+patch-dayone/set-queue-tag "@@icebox")))

(defun +patch-dayone/shelve (&optional pom)
  "Move task to shelf (selected for this year, not yet in quarterly planning).
Clears scheduling, priority, OPENED, and planning properties."
  (interactive)
  (save-excursion
    (when pom (goto-char pom))
    (+patch-dayone/clean-task)
    (org-todo "TODO")
    (+patch-dayone/set-queue-tag "@@shelf")))

(defun +patch-dayone/send-to-backburner (&optional pom)
  "Move task to backburner (pulled into this quarter).
Sets OPENED date if not already set."
  (interactive)
  (save-excursion
    (when pom (goto-char pom))
    (org-todo "TODO")
    (+patch-dayone/set-queue-tag "@@backburner")
    (unless (org-entry-get nil "OPENED")
      (+patch/set-opened-date (point) (ts-format (ts-now))))))

(defun +patch-dayone/send-to-frontburner (&optional pom)
  "Move task to frontburner (this week's priorities).
Sets OPENED date if not already set. Clears any existing schedule."
  (interactive)
  (save-excursion
    (when pom (goto-char pom))
    (org-todo "TODO")
    (org-schedule '(4))  ; prefix arg to clear schedule
    (+patch-dayone/set-queue-tag "@@frontburner")
    (unless (org-entry-get nil "OPENED")
      (+patch/set-opened-date (point) (ts-format (ts-now))))))

(defun +patch-dayone/agenda/freeze ()
  "Move agenda task to icebox."
  (interactive)
  (+patch--from-source-of-agenda-entry (+patch-dayone/freeze)))

(defun +patch-dayone/agenda/shelve ()
  "Move agenda task to shelf."
  (interactive)
  (+patch--from-source-of-agenda-entry (+patch-dayone/shelve)))

(defun +patch-dayone/agenda/send-to-backburner ()
  "Move agenda task to backburner."
  (interactive)
  (+patch--from-source-of-agenda-entry (+patch-dayone/send-to-backburner)))

(defun +patch-dayone/agenda/send-to-frontburner ()
  "Move agenda task to frontburner."
  (interactive)
  (+patch--from-source-of-agenda-entry (+patch-dayone/send-to-frontburner)))

(defun +patch-dayone/planning/freeze ()
  "Move task to icebox and refresh planning view."
  (interactive)
  (+patch--from-source-of-agenda-entry (+patch-dayone/freeze))
  (org-ql-view-refresh))

(defun +patch-dayone/planning/shelve ()
  "Move task to shelf and refresh planning view."
  (interactive)
  (+patch--from-source-of-agenda-entry (+patch-dayone/shelve))
  (org-ql-view-refresh))

(defun +patch-dayone/planning/send-to-backburner ()
  "Move task to backburner and refresh planning view."
  (interactive)
  (+patch--from-source-of-agenda-entry (+patch-dayone/send-to-backburner))
  (org-ql-view-refresh))

(defun +patch-dayone/planning/send-to-frontburner ()
  "Move task to frontburner and refresh planning view."
  (interactive)
  (+patch--from-source-of-agenda-entry (+patch-dayone/send-to-frontburner))
  (org-ql-view-refresh))

;; Task type predicates
(setq +patch-dayone/is-task '(not (tags "routine"))
      +patch-dayone/is-action '(not (children))
      ;; NOTE: is-project is currently orphaned but kept for reference/future use
      +patch-dayone/is-project '(and (ancestors "Projects") (children)))

;; Individual queue predicates
(setq +patch-dayone/is-frozen     '(tags "@@icebox")
      +patch-dayone/is-shelved    '(tags "@@shelf")
      +patch-dayone/is-backburner '(tags "@@backburner")
      +patch-dayone/is-frontburner '(tags "@@frontburner")

      ;; Composite predicates
      +patch-dayone/is-in-queue '(or (tags "@@icebox")
                                     (tags "@@shelf")
                                     (tags "@@backburner")
                                     (tags "@@frontburner"))
      +patch-dayone/is-cooking `(and ,+patch-dayone/is-task
                                     (or (tags "@@backburner")
                                         (tags "@@frontburner")))
      +patch-dayone/is-cold `(and ,+patch-dayone/is-task
                                  (or (tags "@@shelf")
                                      (tags "@@icebox"))))

(provide 'dayone-task-queue)
;;; dayone-task-queue.el ends here
