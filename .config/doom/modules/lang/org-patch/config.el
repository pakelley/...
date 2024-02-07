(use-package! org-gtd
  :after (org org-ql)
  ;; TODO this isn't being set properly using :custom, need to debug why when I have a chance
  ;;:custom
  ;;(+patch/org-gtd-tasks-file (concat (file-name-as-directory org-gtd-directory) "org-gtd-tasks.org"))
  :init
  (setq org-gtd-directory "~/.local/share/notes/gtd/")
  (setq org-gtd-process-item-hooks '(org-set-tags-command))
  (setq org-edna-use-inheritance t)
  (org-edna-mode 1)
  (defun +patch/gen-org-refile-rfloc (file headline)
    "Format a specified file/heading for passing to org-refile and org-agenda-refile.
  
   FILE is the file to refile into.
  
   HEADLINE is the headline (inside FILE) to refile into."
    (let ((pos (save-excursion
                 (find-file file)
                 (org-find-exact-headline-in-buffer headline))))
      (list headline file nil pos)))
  
  (defun +patch/refile-to-node (arg file headline)
    (org-agenda-refile arg (+patch/gen-org-refile-rfloc file headline)))
  
  (defun +patch/org-agenda-refile (file headline)
    "Refile item at point to a particular place via org-agenda-refile, but
   with a simpler interface.
  
   FILE is the file to refile into.
  
   HEADLINE is the headline (inside FILE) to refile into."
    (save-window-excursion
      (org-agenda-refile nil (+patch/gen-org-refile-rfloc file headline))))
  
  ;; FIXME setting here instead of in :custom becuase it's not working in :custom (see note above)
  (setq +patch/org-gtd-tasks-file (concat (file-name-as-directory org-gtd-directory) "org-gtd-tasks.org"))
  (defvar +patch/refine-project-map (make-sparse-keymap)
    "Keymap for command `+patch/refine-project-mode', a minor mode.")
  
  (define-minor-mode +patch/refine-project-mode
    "Minor mode for org-gtd."
    nil " +prp" +patch/refine-project-map
    :global nil
    (if +patch/refine-project-mode
        (setq-local
         header-line-format
         (substitute-command-keys
          "\\<+patch/refine-project-mode>Refine project, add subtasks, then press `C-c C-c' to complete."))
      (setq-local header-line-format nil)))
  
  (defun +patch/convert-to-project ()
      (interactive)
      (org-tree-to-indirect-buffer)
      (+patch/refine-project-mode t))
  
  (defun +patch/create-new-project ()
      (interactive)
      (let* ((org-refile-targets `((,+patch/org-gtd-tasks-file :regexp . "*")))
             (rfloc (org-refile-get-location "Parent location for new project")))
        (org-refile t nil rfloc)
        (+org/insert-item-below 1)
        (org-cycle)
        (org-tree-to-indirect-buffer)
        (+patch/refine-project-mode t)))
  
  (defun +patch/refile-to-project ()
      (interactive)
      (let* ((org-refile-targets `((,+patch/org-gtd-tasks-file :regexp . "*")))
             (rfloc (org-refile-get-location "Project to move this task into")))
        (org-refile nil nil rfloc)))
  
  (setq +patch--widen-hooks '())
  
  (defun +patch/widen ()
    (interactive)
    (widen)
    (+patch/refine-project-mode -1)
    (dolist (hook +patch--widen-hooks)
      (save-excursion
        (save-restriction
          (funcall hook)))))
  (map! (:map evil-normal-state-map
              (:prefix-map ("DEL" . "GTD")
               :desc "Archive"             "a" #'org-archive-to-archive-sibling
               :desc "Views"               "V" #'org-ql-view
               :desc "Process Item"        "DEL" #'+patch-gtd/process-inbox-item
               (:prefix ("r" . "Projects")
                :desc "Convert to project" "c" #'+patch/convert-to-project
                :desc "Create new project" "n" #'+patch/create-new-project)))
        (:map +patch/refine-project-map       "C-c C-c" #'+patch/widen)))

(use-package! org-roam
  :after doct
  :custom
  (org-roam-directory "~/.local/share/notes/zettelkasten/")
  (org-roam-completion-everywhere t)
  :config
  (defun doct-org-roam-convert (groups)
    "Convert GROUPS of templates to `org-roam' compatible templates."
    (setq doct-templates
          (mapcar (lambda (template)
                    (if-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                              (org-roam-props (plist-get (plist-get props :doct) :org-roam)))
                        `(,@template ,@org-roam-props)
                      template))
                  (doct-flatten-lists-in groups))))
  
  (defun doct-org-roam--target-file (value)
    "Convert declaration's :file VALUE and extensions to capture template syntax."
    (let (type target)
      ;; TODO: This doesn't catch :olp used together with :datetree
      (when-let ((olp (doct--get :olp)))
        (push :olp type)
        (push olp target))
      (if-let ((head (doct--get :head)))
          (progn
            (push :head type)
            (push (pcase head
                    ((pred stringp) (if (doct--expansion-syntax-p head)
                                        (doct--replace-template-strings
                                         head)
                                      head))
                    ((pred functionp) (doct--fill-template (funcall head)))
                    ((pred doct--list-of-strings-p)
                     (mapconcat (lambda (element)
                                  (if (doct--expansion-syntax-p element)
                                      (doct--fill-template element)
                                    element))
                                head "\n")))
                  target))
        (when-let ((datetree (doct--get :datetree)))
          (push :datetree type)
          (push datetree target)))
      (push :file type)
      (push (doct--type-check :file value '(stringp doct--variable-p)) target)
      `(,(intern (mapconcat (lambda (keyword)
                              (substring (symbol-name keyword) 1))
                            (delq nil type) "+"))
        ,@(delq nil target))))
  
  (defun doct-org-roam--target ()
    "Convert declaration's target to template target."
    (let ((doct-exclusive-target-keywords '(:file :node)))
      (pcase (doct--first-in doct-exclusive-target-keywords)
        ('nil (signal 'doct-no-target `(,doct-exclusive-target-keywords nil ,doct--current)))
        (`(:id ,id) `(id ,(doct--type-check :id id '(stringp))))
        (`(:file ,file) (doct-org-roam--target-file file)))))
  
  (defun doct-org-roam--compose-entry (keys name parent)
    "Return a template suitable for `org-roam-capture-templates'.
  The list is of the form: (KEYS NAME type target template additional-options...).
  `doct--current-plist' provides the type, target template and additional options.
  If PARENT is non-nil, list is of the form (KEYS NAME)."
    `(,keys ,name
            ,@(unless parent
                `(,(doct--entry-type)
                  ,(doct--template)
                  :target ,(doct-org-roam--target)
                  ,@(doct--additional-options)))
            :doct ( :doct-name ,name
                    ,@(cdr doct--current)
                    ,@(when-let ((custom (doct--custom-properties)))
                        `(:doct-custom ,custom)))))
  
  (defun doct-org-roam (declarations)
    "Convert DECLARATIONS to `org-roam-capture-templates'.
  DECLARATIONS must be of the same form that `doct' expects with
  one addition: the :org-roam keyword.
  The :org-roam keyword's value must be a plist mapping `org-roam''s
  template syntax extensions (e.g. :file-name :head) to their appropriate values.
  Note this does validate the :org-roam plist's values or keywords."
  
    ;;TODO: we should preserve doct-after-conversion-functions
    ;;in case user already has other functions set.
    (let ((doct-after-conversion-functions (append '(doct-org-roam-convert)
                                                   doct-after-conversion-functions)))
      (cl-letf (((symbol-function 'doct--compose-entry) #'doct-org-roam--compose-entry))
        (doct declarations))))
  (setq org-roam-completion-system 'default
        org-roam-capture-templates
        (doct-org-roam
         `(:group "Org Roam"
           :file "%<%Y%m%d%H%M%S>-${slug}.org"
           :head "#+title: ${title}\n"
           :unnarrowed t
           :function ignore ;org-roam hardcodes target file logic
           :type plain
           :children
           (("Default"
             :keys "d"
             :template "%?")
            ("Literature Note"
               :keys "n"
               :template "%?"
               ;; :file
               :file "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/${citar-citekey}.org"
               :head "#+title: ${citar-citekey} (${citar-date}). ${note-title}.\n#+created: %U\n#+last_modified: %U\n\n")
            ("Anki Card"
             :keys "a"
             :hook ,(defun set-anki-deck-from-tags ()
                      (let ((tags (completing-read-multiple "Tag: " (org-roam-tag-completions))))
                        (org-roam-tag-add tags)
                                          ; NOTE this only sets the first tag as ANKI_DECK
                        (org-set-property "ANKI_DECK" (car tags))))
             :template ("* ${title}"
                        "%?"))))))
  
  (setq org-roam-dailies-directory "journals/"
        org-roam-dailies-capture-templates
        '(("d" "default" entry
           #'org-roam-capture--get-point
           "* %?"
           :file-name "Journal/%<%Y-%m-%d>"
           :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
          ("t" "Task" entry
           #'org-roam-capture--get-point
           "* TODO %?\n  %U\n  %a\n  %i"
           :file-name "Journal/%<%Y-%m-%d>"
           :olp ("Tasks")
           :empty-lines 1
           :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
          ("j" "journal" entry
           #'org-roam-capture--get-point
           "* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n"
           :file-name "Journal/%<%Y-%m-%d>"
           :olp ("Log")
           :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
          ("l" "log entry" entry
           #'org-roam-capture--get-point
           "* %<%I:%M %p> - %?"
           :file-name "Journal/%<%Y-%m-%d>"
           :olp ("Log")
           :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
          ("m" "meeting" entry
           #'org-roam-capture--get-point
           "* %<%I:%M %p> - %^{Meeting Title}  :meetings:\n\n%?\n\n"
           :file-name "Journal/%<%Y-%m-%d>"
           :olp ("Log")
           :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")))
  ;; start org-roam on startup
  (org-roam-db-autosync-mode))

(after! org-protocol
  (defun org-roam-protocol-open-daily (info)
    (let ((goto (plist-get info :goto))
          (keys (plist-get info :keys)))
      (org-roam-dailies-capture-today goto keys))
    nil)

  (push '("org-roam-daily"  :protocol "roam-daily"   :function org-roam-protocol-open-daily)
        org-protocol-protocol-alist))

(use-package! org-ref
  :defer t
  :config
  (setq bibtex-completion-bibliography "/Users/pakelley/.local/share/bibtex/references.bib"
        bibtex-completion-library-path "/Users/pakelley/.local/share/bibtex/pdfs/"
        bibtex-completion-notes-path "/Users/pakelley/.local/share/bibtex/notes.org")
  (setq reftex-default-bibliography '("/Users/pakelley/.local/share/bibtex/references.bib"))
  (setq org-ref-default-bibliography '("/Users/pakelley/.local/share/bibtex/references.bib")
        org-ref-pdf-directory "/Users/pakelley/.local/share/bibtex/pdfs/"
        org-ref-bibliography-notes "/Users/pakelley/.local/share/bibtex/notes.org"))

(use-package! org-capture
  :after org
  :commands org-capture
  :defer-incrementally doct)
(use-package! doct
  :commands doct
  :defines +patch/doct-properties
  :config
  ;; setq
  (setq org-capture-templates
        (append org-capture-templates
                (doct '(("Inbox"
                         :keys "i"
                         :file "~/.local/share/notes/gtd/inbox.org"
                         :template "* %?"
                         :kill-buffer t)
                        ("Email"
                         :keys "e"
                         :olp ("Email")
                         :file "~/.local/share/notes/gtd/org-gtd-tasks.org"
                         ;; :hook +patch/doct-properties
                         ;; ;; NOTE: Timestamp needs to be inactive (using the third arg
                         ;; ;;       of org-insert-time-stamp) to avoid the OPENED date
                         ;; ;;       appearing in the agenda.
                         ;; :properties (:OPENED "%(org-insert-time-stamp (org-read-date nil t \"+0d\") nil t)")
                         :hook (lambda () (progn
                                            (org-set-tags "@email")
                                            (org-set-property "OPENED" (format-time-string (org-time-stamp-format) (org-read-date nil t "+0d")))))
                         :kill-buffer t
                         :children
                         (("Todo"
                           :keys "t"
                           :template ("* TODO Reply: %a"
                                      "SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))"))
                          ("Wait"
                           :keys "w"
                           :template ("* WAIT %a"))))
                        ("Frontburner"
                         :keys "f"
                         :file "~/.local/share/notes/gtd/org-gtd-tasks.org"
                         :olp ("Calendar")
                         ;; :hook +patch/doct-properties
                         ;; NOTE: Timestamp needs to be inactive (using the third arg
                         ;;       of org-insert-time-stamp) to avoid the OPENED date
                         ;;       appearing in the agenda.
                         ;; :properties (:OPENED "%(org-insert-time-stamp (org-read-date nil t \"+0d\") nil t)")
                         :hook (lambda ()
                                 (org-set-tags "@@frontburner")
                                 (progn (org-set-property "OPENED" (format-time-string (org-time-stamp-format) (org-read-date nil t "+0d"))))
                                 ;; whitespace doesn't seem to be working properly in the template, so put a space between the cursor and the TODO/tags
                                 (insert " ")
                                 (insert " ")
                                 (backward-char))
                         :template "* TODO  %? "
                         ;; :template ("* TODO  %? "
                         ;;            "SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))")
                         :prepare-finalize (lambda () (progn (org-priority)
                                                             (org-set-tags-command)))
                         :kill-buffer t)
                        ("Meeting"
                         :keys "m"
                         :children
                         (("Retro"
                           :keys "r"
                           :file "~/.local/share/notes/meetings/retro.org"
                           :datetree t
                           :template "* %?"
                           :kill-buffer t)
                          ("Nico 1:1"
                           :keys "n"
                           :file "~/.local/share/notes/meetings/nico.org"
                           :datetree t
                           :template "* %?"
                           :kill-buffer t)
                          ("Haotian 1:1"
                           :keys "h"
                           :file "~/.local/share/notes/meetings/haotian.org"
                           :datetree t
                           :template "* %?"
                           :kill-buffer t)
                          ("Parking Lot"
                           :keys "p"
                           :file "~/.local/share/notes/meetings/parking-lot.org"
                           :datetree t
                           :template "* %?"
                           :kill-buffer t)
                          ("Kinso"
                           :keys "k"
                           :file "~/.local/share/notes/meetings/kinso.org"
                           :datetree t
                           :template "* %?"
                           :kill-buffer t)))
                        ("Shopping" :keys "s"
                         :file "~/.local/share/notes/gtd/org-gtd-tasks.org"
                         :template "* %?"
                         :children
                         (("Home" :keys "h" :olp ("Projects" "home improvement"))
                          ("Christmas" :keys "c" :olp ("Projects" "christmas"))
                          ("Gift" :keys "g" :olp ("Projects" "gifts")) ; TODO either add recipient as tag or in olp
                          ("Groceries" :keys "o" :olp ("Projects" "groceries"))))
                        (:group "Reference"
                         :file "~/.local/share/notes/gtd/org-gtd-tasks.org"
                         :template "* %?"
                         :children
                         (("Food"
                           :keys "F"
                           :children
                           (("Recipe"     :keys "r" :olp ("Projects" "recipes"))
                            ("Cocktail"   :keys "c" :olp ("Projects" "cocktails"))
                            ("Restaurant" :keys "s" :olp ("Projects" "restaurants"))))
                          ("Media" :keys "d"
                           :children
                           (("Movie"   :keys "m" :olp ("Projects" "movies"))
                            ("Show"    :keys "s" :olp ("Projects" "shows"))
                            ("Book"    :keys "b" :olp ("Projects" "books"))
                            ("Article" :keys "a" :olp ("Projects" "articles"))
                            ("Album"   :keys "l" :olp ("Projects" "albums"))))
                          ("Repo" :keys "r" :olp ("Projects" "repos"))))))))
  (defun +patch/doct-properties ()
    "Add declaration's :properties to current entry."
    (let ((properties (doct-get :properties)))
      (dolist (keyword (seq-filter #'keywordp properties))
        (let* ((property (substring (symbol-name keyword) 1))
               (raw-value (plist-get properties keyword))
               (expanded-value (if (string-match-p ".*%(.*" raw-value)
                          (org-capture-fill-template raw-value)
                        raw-value))
               (clean-value (replace-regexp-in-string "\n$" "" expanded-value)))
          (org-set-property property clean-value)))))
  ;; Usage:
  ;; (doct '(("My capture template"
  ;;          ...
  ;;          :hook +patch/org-property-drawer
  ;;          :properties (:anki_deck "${category}"))))
  )

(after! emacs-everywhere
  (defun get-app-name ()
    "Get the name of the current app (useful for returning to that app later). Currently uses osascript, so only useful on macos."
    (let ((app-name (emacs-everywhere-app-id (emacs-everywhere-app-info))))
      ;; For some reason, wezterm returns "wezterm-gui" for its app name, but
      ;; osascript can only find "wezterm".
      (if (equal app-name "wezterm-gui")
          "wezterm"
        app-name)))

  (defun capture-everywhere ()
    "Create a new frame and run `org-capture'."
    (interactive)
    (require 'noflet)
    (make-frame `((name . "capture")
                  (top . 300)
                  (left . 700)
                  (width . 80)
                  (height . 25)
                  (emacs-everywhere-prior-app . ,(get-app-name))))

    (select-frame-by-name "capture")
    (delete-other-windows)
    (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
      (org-capture)))


  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame and return to the app we came from"
    (when (and emacs-everywhere-window-focus-command (frame-parameter nil 'emacs-everywhere-prior-app))
      (apply #'call-process (car emacs-everywhere-window-focus-command)
             nil nil nil
             (mapcar (lambda (arg)
                       (when-let ((prior-app (frame-parameter nil 'emacs-everywhere-prior-app))) (replace-regexp-in-string "%w" prior-app arg)))
                     (cdr emacs-everywhere-window-focus-command))))
    (when (frame-parameter nil 'emacs-everywhere-prior-app)
      (delete-frame))))

(use-package! org-agenda
  :commands org-agenda
  :custom
  (org-agenda-files '("~/.local/share/notes/gtd/org-gtd-tasks.org"))
  :config
  (setq org-agenda-prefix-format
        '((agenda . "  %?-12t")
          (todo   . " ")
          ;; should maybe come back to these next two, but haven't had a need for it yet
          (tags   . " %i %-12:c")
          (search . " %i %-12:c")))
  (defmacro η (fnc)
    "Return function that ignores its arguments and invokes FNC."
    `(lambda (&rest _rest)
       (funcall ,fnc)))
  
  (advice-add 'org-deadline       :after (η #'org-save-all-org-buffers))
  (advice-add 'org-schedule       :after (η #'org-save-all-org-buffers))
  (advice-add 'org-store-log-note :after (η #'org-save-all-org-buffers))
  (advice-add 'org-todo           :after (η #'org-save-all-org-buffers))
  (advice-add 'org-refile         :after (η #'org-save-all-org-buffers))
  ;; (run-with-idle-timer 300 t (lambda () (save-window-excursion (org-agenda nil ","))))
  (defun +patch-dayone/agenda/reschedule-to-today (&optional arg)
    "Reschedule selected task(s) for today."
    (interactive "P")
    (org-agenda-schedule arg "."))
  
  (defun +patch-dayone/agenda/reschedule-to-tomorrow (&optional arg)
    "Reschedule selected task(s) for tomorrow."
    (interactive "P")
    (org-agenda-schedule arg "+1d"))
  
  (defun +patch-dayone/agenda/unschedule ()
    (interactive)
    (org-agenda-schedule '(4)))
  
  (setq org-agenda-bulk-custom-functions '((?. +patch-dayone/agenda/reschedule-to-today)
                                           (?< +patch-dayone/agenda/unschedule)
                                           (?> +patch-dayone/agenda/reschedule-to-tomorrow)))
  (map! (:map org-agenda-mode-map "." #'+patch-dayone/agenda/reschedule-to-today)
        (:map evil-org-agenda-mode-map :m "." #'+patch-dayone/agenda/reschedule-to-today)
        (:map org-agenda-mode-map "<" #'+patch-dayone/agenda/unschedule)
        (:map evil-org-agenda-mode-map :m "<" #'+patch-dayone/agenda/unschedule)
        (:map org-agenda-mode-map ">" #'+patch-dayone/agenda/reschedule-to-tomorrow)
        (:map evil-org-agenda-mode-map :m ">" #'+patch-dayone/agenda/reschedule-to-tomorrow))
  (map! (:map org-agenda-mode-map
              ("C-RET" (cmd! (org-capture nil "f")))
              ("C-<return>" (cmd! (org-capture nil "f"))))
        (:map evil-org-agenda-mode-map :m
              "C-RET" nil)
        (:map evil-org-agenda-mode-map :m
              "C-<return>" nil)
        (:map org-super-agenda-header-map :m
              "C-RET" nil)
        (:map org-super-agenda-header-map :m
              "C-<return>" nil)
        (:map global-map
              ("C-RET" nil)
              ("C-<return>" nil))
        (:map evil-insert-state-map
              ("C-RET" nil)
              ("C-<return>" nil))
        (:map evil-normal-state-map
              ("C-RET" nil)
              ("C-<return>" nil))))

(use-package! org-refile
  :after org-agenda
  :config
  ;; (add-to-list 'org-refile-targets `(,(directory-files "~/.local/share/notes/reference" t ".*\\.org$") :maxlevel . 3))
  (add-to-list 'org-refile-targets `(,(directory-files "~/.local/share/notes/gtd" t ".*\\.org$") :maxlevel . 3)))

(use-package! org-super-agenda
  :after (org-ql org-agenda)
  :commands org-super-agenda-mode
  :hook (org-agenda-mode . org-super-agenda-mode)
  :custom
  (org-agenda-include-deadlines t)
  (org-agenda-tags-column 100) ;; from testing this seems to be a good value
  (org-agenda-compact-blocks t)
  (org-agenda-custom-commands
   `(
     ("," "Today"
      ((agenda "" ((org-agenda-span 'day)
                   (org-agenda-start-day "+0d")
                   (org-super-agenda-groups
                    '((:name "Agenda"
                       :time-grid t
                       :and (:scheduled today
                             :regexp ,org-ql-regexp-scheduled-with-time
                             :not (:todo ("DONE" "CNCL" "WAIT")))
                       :order 0)
                      (:name "Remove anything else"
                       :discard (:anything t))))))
       ;; overdue
       (org-ql-block '(and (scheduled :to -1)
                           (not (children)) ; only look at actions, not projects
                           (todo "NEXT" "TODO")
                           (not (done))
                           (not (tags "routine")))
                     ((org-ql-block-header "\n Overdue")))
       ;; routine
       (org-ql-block '(and (scheduled :to today)
                           (not (children)) ; only look at actions, not projects
                           (not (todo "DONE" "CNCL" "WAIT" "INCUBATE"))
                           (tags "routine")
                           (regexp ,org-ql-regexp-scheduled-without-time))
                     ((org-ql-block-header "\n Routine")))
       ;; todos
       (org-ql-block '(and (scheduled :on today)
                           (not (children)) ; only look at actions, not projects
                           (not (todo "DONE" "CNCL" "WAIT" "INCUBATE"))
                           (not (tags "routine"))
                           (regexp ,org-ql-regexp-scheduled-without-time))
                     ((org-ql-block-header "\n Todo")))
       ;; frontburner
       (org-ql-block '(and (tags "@@frontburner")
                           (not (scheduled))
                           (not (done)))
                     ((org-ql-block-header "\n Frontburner")))
       ;; jira
       (org-ql-block
        '(and
          (not (regexp ,org-ql-regexp-scheduled-with-time))
          (or
           ;; my work
           (and (property "assignee" "Patrick Kelley")
                (todo "READY" "TODO"))
           ;; needs review
           (property "status" "In Review")
           ;; (todo "REVIEW")
           ;; needs to be deployed
           (property "status" "Confirmed")))
        ;; (todo "CONFIRMED"))
        ((org-ql-block-header "\n Jira")))
       ;; waiting
       (org-ql-block '(todo "WAIT")
                     ((org-ql-block-header "\n Waiting")))
     
       ;; completed today
       (org-ql-block '(closed :on today)
                     ((org-ql-block-header "\n Completed today")))))
     )))

(after! evil-org-agenda
  (setq org-super-agenda-header-map (copy-keymap evil-org-agenda-mode-map))
  (map!
   (:map org-agenda-keymap "j" #'evil-next-line)
   (:map org-agenda-mode-map "j" #'evil-next-line)
   (:map org-agenda-keymap "k" #'evil-previous-line)
   (:map org-agenda-mode-map "k" #'evil-previous-line)))

(use-package! org-ql
  :after (org-agenda ts)
  :custom
  (org-super-agenda-date-format "%e %B %Y - %A")
  :defines (+patch/set-orgql-view +patch/is-action)
  :config
  ;; have to setq instead of :custom bc we need access to org-ql vars (so we need it executed after the package is loaded, and :custom seems to be executed before the package is loaded)
  (setq
    +patch/is-project '(and (ancestors "Projects") (children))
    +patch/is-action '(not (children)))
  
  (defun +patch--get-path (task)
    "Try to find the path to TASK by walking up ':parent' tasks (found using the
  org element API), then getting the ':path' property of the top."
    (when task
      (or (org-element-property :path task)
          (+patch--get-path (org-element-property :parent task)))))
  
  ;; TODO need to compare the behavior of this with +patch--from-source-of-agenda-entry, and consolidate if possible
  (defmacro +patch--from-task-location (task &rest body)
    "Runs BODY from the buffer of the task specified by 'task'. This will try to
  find the buffer via the ':org-marker' property in the org element api, or by
  walking up ':parent' tasks until the top, and getting the ':path' property and
  getting a buffer (opening if necessary) for that file"
    `(let ((buffer (if-let ((marker (org-element-property :org-marker ,task)))
                       (marker-buffer marker)
                     (find-file-noselect (+patch--get-path task)))))
       (with-current-buffer buffer
         (org-mode)
         ,@body)))
  
  ;; TODO leaving bc this seems somewhat general-purpose, but not sure if I actually need it (it isn't being used anywhere)
  (defun +patch--get-contents (task)
    "Using the org element API, get the contents of a task (i.e. the plain text
  after the headline).
  
  If there's a marker, use that (because it's more robust), otherwise recursively
  search up the task tree for a ':path' property (and hope for the best)."
    (+patch--from-task-location task
                                (let* ((beg (org-element-property :contents-begin task))
                                       (end (org-element-property :contents-end task)))
                                  (when beg (when end (buffer-substring-no-properties beg end))))))
  
  (defun +patch/get-opened-date (task)
    "Get the date a task was opened (i.e. moved from READY to TODO/NEXT) using
  the org element api. Requires fetching the content of the task (which I don't
  have a reliable process for yet)."
    (let* ((opened-prop (org-element-property :OPENED task)))
      (when opened-prop
        (let* ((opened-ts (ts-parse opened-prop))
               (opened-date (ts-format "%Y-%m-%d" opened-ts)))
          opened-date))))
  
  (defun +patch/get-quarter-planned-date (task)
    "Get the quarter a task was planned during using
  the org element api. Requires fetching the content of the task (which I don't
  have a reliable process for yet)."
    (let* ((opened-prop (org-element-property :OPENED task)))
      (when opened-prop
        (let* ((opened-ts (ts-parse opened-prop))
               (opened-date (ts-format "%Y-%m-%d" opened-ts)))
          opened-date))))
  
  ;; TODO want to move this definition to quarterly planning utils (bc it's a natural place for the definition), but I depend on it for my transient for inbox processing and need to sort out the load order.
  (defun +patch/set-opened-date (&optional pom date)
    "Set the OPENED date of a task."
    (interactive)
    (let* ((pom (or pom (point)))
           (date (or date (org-read-date)))
           (date-str (ts-format "%Y-%m-%d" (ts-parse date))))
      (org-entry-put pom "OPENED" date-str)))
  
  ;; TODO move this to a more general location
  (defmacro +patch--from-source-of-agenda-entry (&rest body)
    "Goes to org file entry that corresponds to the entry at point in an agenda
  view, and runs BODY. Implementation largely taken from the
  'org-agenda-set-property' implementation."
    `(let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                          (org-agenda-error)))
            (buffer (marker-buffer hdmarker))
            (pos (marker-position hdmarker))
            (inhibit-read-only t)
            ) ;; newhead
       ;; need to get any return inside `org-with-remote-undo`, bc it won't pass it along
       (let ((result))
         (org-with-remote-undo buffer
           (setq result
                 (with-current-buffer buffer
                   (goto-char pos)
                   (ignore-errors (org-mode))
                   (widen)
                   (org-fold-show-context 'agenda)
                   ,@body)))
         result)))
  (org-ql-defpred opened (&key from to _on)
    "Return non-nil if current entry contains READY state change in given period."
    :normalizers ((`(,predicate-names ,(and num-days (pred numberp)))
                   ;; (clocked) and (closed) implicitly look into the past.
                   (let* ((from-day (* -1 num-days))
                          (rest (list :from from-day)))
                     (org-ql--normalize-from-to-on
                       `(opened :from ,from))))
                  (`(,predicate-names . ,rest)
                   (org-ql--normalize-from-to-on
                     `(opened :from ,from :to ,to))))
    :preambles
    ((`(,predicate-names . ,rest)
      (list
       ;; Predicate needs testing only when args are present.
       :query (-let (((&keys :from :to :on) rest))
                ;; TODO: This used to be (when (or from to on) query), but
                ;; that doesn't seem right, so I changed it to this if, and the
                ;; tests pass either way.  Might deserve a little scrutiny.
                (if (or from to on)
                    query
                  t)))))
    :body
      (when-let ((opened-prop (org-entry-get (point) "OPENED")))
        (let ((opened-at (ts-parse opened-prop)))
          (save-excursion
            (cond ((not (or from to)) opened-at)
                  ((and from to) (ts-in from to opened-at))
                  (from (ts<= from opened-at))
                  (to (ts<= opened-at to))))))
    )
  (setq
   +patch/daily-agenda-super-groups
   `((:name "Routine"
      :time-grid t
      :and (:scheduled today
            :tag "routine"
            :not (:tag ("%quick" "%easy"))
            :not (:todo ("DONE" "CNCL" "WAIT")))
      :order 0)
     (:name "Todo"
      :time-grid t
      :and (:scheduled today
            :not (:tag ("%quick" "%easy"))
            :not (:tag "routine")
            :not (:todo ("DONE" "CNCL" "WAIT")))
      :order 0)
     (:name "Quick"
      :and (:tag "%quick"
            :scheduled today
            :not (:todo ("DONE" "CNCL" "WAIT"))
            :not (:regexp ,org-ql-regexp-scheduled-with-time)))
     (:name "Easy"
      :and (:tag "%easy"
            :scheduled today
            :not (:todo ("DONE" "CNCL" "WAIT"))
            :not (:regexp ,org-ql-regexp-scheduled-with-time)))
     (:name "Overdue"
      :and (:scheduled past
            :face error
            :not (:todo ("DONE" "CNCL" "WAIT"))))
     (:name "Waiting"
      :todo "WAIT")
     (:name "Completed Today"
      ;; TODO would be nice to include "CLOSED" today, rather than basing on scheduled time (but :log closed doesn't seem to be working for me)
      :and (:todo "DONE"
            :scheduled today))
     (:name "Could Pull In"
      :and (:tag ("%quick" "%easy")
            ;; scheduled in the next 3 days
            :scheduled future
            :scheduled (before ,(org-read-date nil nil "+4"))))
     (:name "Remove anything else"
      :discard (:anything t)))

   +patch/daily-agenda-query
   '(and (or (ts-active :on today)
             (scheduled :to +3)
             (scheduled :before today))
         (not (children))
         (not (todo "CNCL")))

   org-ql-views
   `(("Home"
      :buffers-files ("~/.local/share/notes/gtd/org-gtd-tasks.org")
      :query '(and (tags "@home" "@work" "@anywhere")
                   ,+patch/daily-agenda-query)
      :sort (priority todo date)
      :narrow nil
      :super-groups ,+patch/daily-agenda-super-groups
      :title "Home")
     ("Work"
      :buffers-files ("~/.local/share/notes/gtd/org-gtd-tasks.org")
      :query '(and (tags "@work" "@anywhere")
                   ,+patch/daily-agenda-query)
      :sort (priority todo date)
      :narrow nil
      :super-groups ,+patch/daily-agenda-super-groups
      :title "Work")
     ("Email"
      :buffers-files ("~/.local/share/notes/gtd/org-gtd-tasks.org")
      :query '(and (tags "@email")
                   ,+patch/daily-agenda-query)
      :sort (priority todo date)
      :narrow nil
      :super-groups ,+patch/daily-agenda-super-groups
      :title "Email")))
       (defun +patch/set-orgql-view (view-name view-spec)
         (let ((view (assoc view-name org-ql-views)))
           (if view
               (setf (cdr view) view-spec)
             (add-to-list 'org-ql-views `(,view-name . ,view-spec)))))
       (defun +patch-gtd/set-or-refresh-yearly-views ()
         (setq
          +patch-dayone/is-active '(and (todo "TODO" "NEXT")
                                      (not (tags "routine")))
          +patch/is-top-level-selected-task '(and (todo "TODO" "NEXT")
                                                  (not (tags "routine"))
                                                  (not (ancestors (todo "TODO" "NEXT")))))
       
         (+patch/set-orgql-view
          "Active Tasks"
          `(:buffers-files ("~/.local/share/notes/gtd/org-gtd-tasks.org")
            :query ,+patch-dayone/is-active
            :sort (priority todo)
            :narrow nil
            :super-groups ((:auto-outline-path t))
            :title "Active Tasks"))
       
         ;; hoping to get rid of this, but leaving it for now
         (+patch/set-orgql-view
          "Active Tasks Schedule"
          `(:buffers-files ("~/.local/share/notes/gtd/org-gtd-tasks.org")
            :query ,+patch/is-top-level-selected-task
            :sort (priority todo)
            :narrow nil
            :super-groups ((:auto-planning t))
            :title "Yearly Planning"))
       
         ;; hoping to get rid of this, but leaving it for now
         (+patch/set-orgql-view
          "Active Projects" ;; previously "Yearly Planning"
          `(:buffers-files ("~/.local/share/notes/gtd/org-gtd-tasks.org")
            :query (and ,+patch/is-top-level-selected-task ,+patch/is-project)
            :sort (priority todo)
            :narrow nil
            :super-groups ((:auto-outline-path t))
            :title "Active Projects")))
       
       (+patch-gtd/set-or-refresh-yearly-views)
       (defun +patch/start-of-this-quarter-ts (&optional as-of)
         (let* ((base-ts (or as-of (ts-now)))
                (base-date (ts-apply :hour 0 :minute 0 :second 0 base-ts))
                ;; (this-month (ts-month base-date))
                (this-quarter (+patch/ts-quarter base-date))
                (last-month-of-quarter (* this-quarter 3)))
           (ts-dec 'month 2 (ts-apply :month last-month-of-quarter :day 1 base-date))))
       
       (defun +patch/start-of-this-year-ts (&optional as-of)
         (let* ((base-ts (or as-of (ts-now))))
           (ts-apply :month 1 :day 1 :hour 0 :minute 0 :second 0 base-ts)))
       
       
       
       (defun +patch/end-of-this-quarter-ts (&optional as-of)
         (let* ((base-ts (or as-of (ts-now)))
                (base-date (ts-apply :hour 0 :minute 0 :second 0 base-ts))
                (this-quarter (+patch/ts-quarter base-date))
                (last-month-of-quarter (* this-quarter 3))
                (first-month-of-next-quarter (ts-inc 'month 1 (ts-apply :month last-month-of-quarter :day 1 base-date))))
           (ts-dec 'second 1 first-month-of-next-quarter)))
       
       (defun +patch/start-of-last-quarter-ts (&optional as-of)
         (let* ((base-ts (or as-of (ts-now)))
                (start-of-this-quarter (+patch/start-of-this-quarter-ts as-of)))
           (ts-dec 'month 3 start-of-this-quarter)))
       
       (defun +patch/end-of-last-quarter-ts (&optional as-of)
         (let* ((base-ts (or as-of (ts-now)))
                (start-of-this-quarter (+patch/start-of-this-quarter-ts as-of)))
           (ts-dec 'second 1 start-of-this-quarter)))
       
       
       (defun +patch-gtd/set-or-refresh-quarterly-views ()
         (setq
          +patch-dayone/is-open `(and ,+patch-dayone/is-active (property "OPENED"))
          +patch-dayone/has-been-open `(and (not (tags "routine")) (property "OPENED"))
          +patch-dayone/is-unopened-active-task `(and ,+patch-dayone/is-active (not (property "OPENED")))
          +patch-dayone/closed-before-this-quarter `(closed :to ,(ts-format (+patch/start-of-this-quarter-ts)))
          +patch-dayone/planned-for-this-quarter `(and ,+patch-dayone/has-been-open
                                                       (not ,+patch-dayone/closed-before-this-quarter))
          +patch-dayone/closed-before-this-year `(closed :to ,(ts-format (ts-dec 'second 1 (+patch/start-of-this-year-ts))))
          +patch-dayone/planned-for-this-year `(and ,+patch-dayone/has-been-open
                                                       (not ,+patch-dayone/closed-before-this-year))
       )
       
         (defun +patch/num-tasks-completed-last-quarter (&optional as-of)
           (length
            (org-ql-query
              :from (cons "~/.local/share/notes/gtd/org-gtd-tasks.org" (f-glob "gtd_archive_[0-9][0-9][0-9][0-9]" "~/.local/share/notes/gtd"))
              :where `(closed :from ,(ts-format (+patch/start-of-last-quarter-ts as-of)) :to ,(ts-format (+patch/end-of-last-quarter-ts as-of))))))
       
         (defun +patch/num-tasks-planned-for-this-quarter (&optional as-of)
           (length
            (org-ql-query
              :from (cons "~/.local/share/notes/gtd/org-gtd-tasks.org" (f-glob "gtd_archive_[0-9][0-9][0-9][0-9]" "~/.local/share/notes/gtd"))
              :where `(opened :from ,(ts-format (+patch/start-of-this-quarter-ts as-of)) :to ,(ts-format (+patch/end-of-this-quarter-ts as-of))))))
       
         (+patch/set-orgql-view
          "Backburner"
          `(:buffers-files ("~/.local/share/notes/gtd/org-gtd-tasks.org")
            :query (and ,+patch-dayone/is-open ,+patch/is-action)
            :sort (priority todo)
            :narrow nil
            :super-groups ((:auto-outline-path t))
            :title "Backburner"))
       
         (+patch/set-orgql-view
          "Unopened Active Tasks"
          `(:buffers-files ("~/.local/share/notes/gtd/org-gtd-tasks.org")
            :query ,+patch-dayone/is-unopened-active-task
            :sort (priority todo)
            :narrow nil
            :super-groups ((:auto-outline-path t))
            :title ,(format "[Completed last quarter: %s] [Planned for this quarter: %s]" (+patch/num-tasks-completed-last-quarter) (+patch/num-tasks-planned-for-this-quarter))
            )))
       
       (+patch-gtd/set-or-refresh-quarterly-views)
       
       (defun +patch-gtd/set-or-refresh-weekly-views ()
         (setq
          scheduled-for-this-week (let* ((today (ts-apply :hour 0 :minute 0 :second 0 (ts-now)))
                                         (dow (ts-day-of-week-num today))
                                         (start-of-week (ts-dec 'day dow today))
                                         (start-of-next-week (ts-inc 'day (- 6 dow) (ts-now)))
                                         (end-of-week (ts-dec 'second 1 start-of-next-week)))
                                    `(scheduled
                                      :from ,(ts-format start-of-week)
                                      :to ,(ts-format end-of-week)))
          scheduled-around-this-week (let* ((today (ts-apply :hour 0 :minute 0 :second 0 (ts-now)))
                                            (dow (ts-day-of-week-num today))
                                            (start-of-week (ts-dec 'day dow today))
                                            (start-of-period (ts-dec 'day 3 start-of-week))
                                            (start-of-next-week (ts-inc 'day (- 6 dow) (ts-now)))
                                            (end-of-week (ts-dec 'second 1 start-of-next-week))
                                            (end-of-period (ts-inc 'day 3 end-of-week)))
                                       `(scheduled
                                         :from ,(ts-format start-of-period)
                                         :to ,(ts-format end-of-period)))
          scheduled-til-this-week (let* ((today (ts-apply :hour 0 :minute 0 :second 0 (ts-now)))
                                         (dow (ts-day-of-week-num today))
                                         (start-of-week (ts-dec 'day dow today)))
                                    `(scheduled
                                      :to ,(ts-format start-of-week)))
          scheduled-through-this-week (let* ((today (ts-apply :hour 0 :minute 0 :second 0 (ts-now)))
                                             (dow (ts-day-of-week-num today))
                                             (start-of-week (ts-dec 'day dow today))
                                             (start-of-next-week (ts-inc 'day (- 6 dow) (ts-now)))
                                             (end-of-week (ts-dec 'second 1 start-of-next-week))
                                             (end-of-period (ts-inc 'day 3 end-of-week)))
                                        `(scheduled
                                          :to ,(ts-format end-of-period)))
          scheduled-through-around-this-week (let* ((today (ts-apply :hour 0 :minute 0 :second 0 (ts-now)))
                                                    (dow (ts-day-of-week-num today))
                                                    (start-of-week (ts-dec 'day dow today))
                                                    (start-of-next-week (ts-inc 'day (- 6 dow) (ts-now)))
                                                    (end-of-week (ts-dec 'second 1 start-of-next-week)))
                                               `(scheduled
                                                 :to ,(ts-format end-of-week)))
          opened-through-this-week (let* ((today (ts-apply :hour 0 :minute 0 :second 0 (ts-now)))
                                          (dow (ts-day-of-week-num today))
                                          (start-of-week (ts-dec 'day dow today))
                                          (start-of-next-week (ts-inc 'day (- 6 dow) (ts-now)))
                                          (end-of-week (ts-dec 'second 1 start-of-next-week)))
                                     `(opened
                                       :to ,(ts-format end-of-week)))
          due-this-week (let* ((today (ts-apply :hour 0 :minute 0 :second 0 (ts-now)))
                               (dow (ts-day-of-week-num today))
                               (start-of-week (ts-dec 'day dow today))
                               (start-of-next-week (ts-inc 'day (- 6 dow) (ts-now)))
                               (end-of-week (ts-dec 'second 1 start-of-next-week)))
                          `(deadline :from ,(ts-format start-of-week) :to ,(ts-format end-of-week)))
          due-through-around-this-week (let* ((today (ts-apply :hour 0 :minute 0 :second 0 (ts-now)))
                                              (dow (ts-day-of-week-num today))
                                              (start-of-week (ts-dec 'day dow today))
                                              (start-of-period (ts-dec 'day 3 start-of-week))
                                              (start-of-next-week (ts-inc 'day (- 7 dow) (ts-now)))
                                              (start-of-next-period (ts-inc 'day 3 start-of-next-week)))
                                         `(deadline :to ,(ts-format start-of-next-period)))
          due-this-week-sa (let* ((today (ts-apply :hour 0 :minute 0 :second 0 (ts-now)))
                                  (dow (ts-day-of-week-num today))
                                  (start-of-week (ts-dec 'day dow today))
                                  (start-of-next-week (ts-inc 'day (- 7 dow) (ts-now)))
                                  (end-of-week (ts-dec 'second 1 start-of-next-week)))
                             `(before ,(prin1-to-string (ts-format "%Y-%m-%d" start-of-next-week))))
          due-around-this-week-sa (let* ((today (ts-apply :hour 0 :minute 0 :second 0 (ts-now)))
                                         (dow (ts-day-of-week-num today))
                                         (start-of-week (ts-dec 'day dow today))
                                         (start-of-period (ts-dec 'day 3 start-of-week))
                                         (start-of-next-week (ts-inc 'day (- 7 dow) (ts-now)))
                                         (start-of-next-period (ts-inc 'day 3 start-of-next-week)))
                                    `(before ,(prin1-to-string (ts-format "%Y-%m-%d" start-of-next-period)))))
       
         (+patch/set-orgql-view
          "Non-Scheduled Backburner"
          `(:buffers-files ("~/.local/share/notes/gtd/org-gtd-tasks.org")
            :query (and
                    ,+patch-dayone/is-open
                    ,+patch/is-action
                    (not ,scheduled-through-around-this-week))
            :sort (priority todo)
            :narrow nil
            :super-groups ((:auto-outline-path t))
            :title "Backburner (non-scheduled)"))
       
         (+patch/set-orgql-view
          "This Week's Agenda"
          `(:buffers-files ("~/.local/share/notes/gtd/org-gtd-tasks.org")
            :query (and
                    ,+patch-dayone/is-open
                    ,+patch/is-action
                    (or ,scheduled-through-around-this-week
                        ,due-through-around-this-week
                        (tags "@@frontburner")))
            :sort (priority todo)
            :narrow nil
            :super-groups ((:name "Upcoming Deadline"
                            :and (:deadline ,due-around-this-week-sa
                                  :not (:todo ("DONE" "CNCL" "WAIT"))
                                  :not (:scheduled future))
                            :face error
                            :order 0)
                           (:name "Overdue"
                            :and (:scheduled past
                                  :not (:todo ("DONE" "CNCL" "WAIT")))
                            :face error
                            :order 0)
                           (:auto-planning t)
                           (:name "--------------------------------------------------------------------------------------------\nFrontburner"
                            :tag "@@frontburner"
                            :order 9))
            :title "This Week's Agenda"))
         )
       
       (+patch-gtd/set-or-refresh-weekly-views)


  (defun org-ql-action-list (action-list-name)
    (interactive (list (completing-read "Action List: " (--filter (string-match-p "^\@.*" it) (mapcar #'car org-tag-alist)))))
    (org-ql-search "~/.local/share/notes/gtd/org-gtd-tasks.org"
      `(and ,+patch/daily-agenda-query
            (tags "@anywhere" ,action-list-name))
      :title (format "%s action list" action-list-name)
      :super-groups +patch/daily-agenda-super-groups))

  (defun org-ql-refine-view (query)
    (interactive "xQuery: ")
    (let ((org-ql-view-query `(and ,query ,org-ql-view-query)))
      (org-ql-view-refresh))))

(use-package! ox-pandoc
  :after ox
  :custom (org-pandoc-command "/usr/local/bin/pandoc"))
  ;; m1 path
  ;; :custom (org-pandoc-command "/opt/homebrew/bin/pandoc"))

(use-package! deft
  :after org
  :custom
  (deft-directory "~/.local/share/notes")
  (deft-recursive t))

;; (setenv "PATH" (concat ":/Library/TeX/texbin/" (getenv "PATH")))
(add-to-list 'exec-path "/Library/TeX/texbin/")

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
         (replace-regexp-in-string "[[:space:]\n]*$" ""
           (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))

;(setq org-startup-truncated nil)
;(setq org-startup-indented t)

;(setq org-refile-targets
;      '((nil :maxlevel . 3)
;        (org-agenda-files :maxlevel . 3)))

(after! org
  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  (defun +patch/invoke-babel-named (file-path block-name)
    (interactive)
    (save-excursion
      (with-current-buffer (find-file-noselect file-path)
        (org-babel-goto-named-src-block block-name)
        (org-babel-execute-src-block-maybe))))
  
  (defvar +patch/babel-hashes 'nil)
  (defun +patch/babel-hashed-confirm (lang body)
    (let ((check (list lang (md5 body))))
      ;; If not hashed, prompt
      (if (not (member (list lang (md5 body)) +patch/babel-hashes))
          ;; Ask if you want to hash
          (if (yes-or-no-p "Store hash for block? ")
              ;; Hash is added, proceed with evaluation
              (progn
                (add-to-list '+patch/babel-hashes check)
                'nil)
            ;; Return 't to prompt for evaluation
            't))))
  
  (setq org-confirm-babel-evaluate '+patch/babel-hashed-confirm))

(use-package! ob-mermaid
  :defer t
  :config
  (setq ob-mermaid-cli-path "/usr/local/bin/mmdc"))

(use-package! org-pomodoro
  :after org-agenda
  :custom
  ; my personal pomodoro lengths
  (org-pomodoro-length 40)
  (org-pomodoro-short-break-length 10)
  (org-pomodoro-long-break-length 30)
  ; wait for me to start my break
  (org-pomodoro-manual-break t)
  ; only record pomodoro-approved time: overtime doesn't get clocked
  (org-pomodoro-overtime-hook '(org-clock-out))
  ; dont use annoying multiple bell after long break
  (org-pomodoro-long-break-sound org-pomodoro-short-break-sound)
  :config
  (setq org-agenda-clockreport-parameter-plist
     `(:link t :maxlevel 2 :formula ,(format "$5=ceil(($3+$4)*60/%s);N" org-pomodoro-length)))
  (defun +org/switch-task (&optional arg)
    (interactive "P")
    (org-agenda-clock-out)
    (org-agenda-clock-in arg))
  (map! :after org-agenda
        :leader
        (:prefix "n"
         :desc "pomodoro" "p" #'org-pomodoro)
        :map org-agenda-mode-map
        :localleader
        (:prefix ("c" . "clock")
         :desc "switch task" "w" #'+org/switch-task
         :desc "pomodoro" "p" #'org-pomodoro)))

;;(use-package! parinfer-rust-mode
;;  :after parinfer
;;  :custom
;;  (parinfer-rust-check-before-enable nil))

;;(use-package! poly-org
;;  :after org)

(after! org-superstar
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-superstar-prettify-item-bullets t))

(after! (org-fancy-priorities all-the-icons)
  (setq org-ellipsis " ▾ "
        org-hide-leading-stars t
        org-priority-highest ?A
        org-priority-lowest ?E
        org-fancy-priorities-list
        `(,(list ?A (all-the-icons-octicon "flame" :face 'all-the-icons-red))
          ,(list ?B (all-the-icons-faicon "bolt" :face 'all-the-icons-orange))
          ,(list ?C (all-the-icons-faicon "check" :face 'all-the-icons-yellow))
          ,(list ?D (all-the-icons-faicon "beer" :face 'all-the-icons-green))
          ,(list ?E (all-the-icons-faicon "bed" :face 'all-the-icons-blue)))))

(use-package! org-modern
  :after org
  :hook
  (org-mode . org-modern-mode)
  ;; until I figure out how to keep org-modern from inverting face on agenda priorities, leave off org-modern-agenda
  ;;(org-agenda . org-modern-agenda)
  :custom
  (org-modern-priority nil)
  (org-modern-internal-target `(,(all-the-icons-material "redo" :face 'all-the-icons-blue) t " "))
  (org-modern-star ["◉" "○" "✸" "✿" "✤" "✜" "◆"])
  (org-modern-todo-faces
        '(("INCUBATE" . (:background "#dfaf8f" :foreground "black" :weight semibold))
          ("READY"    . (:background "#8cd0d3" :foreground "black" :weight semibold))
          ("NEXT"     . (:background "#f0dfaf" :foreground "black" :weight semibold))
          ("WAIT"     . (:background "#dc8cc3" :foreground "black" :weight semibold))
          ("CNCL"     . (:background "#d26478" :foreground "black" :weight semibold))))

  (org-modern-list '((43 . "➤")
                     (45 . "–")
                     (42 . "•"))))

(use-package! org
  :commands org-mode
  :config
  (setq org-tag-alist '((:startgroup . nil)
                        ("@anywhere"       . ?a)
                        ("@phone"          . ?o)
                        ("@email"          . ?m)
                        ("@book"           . ?b)
                        ("@cheryls"        . ?y)
                        ("@parents"        . ?p)
                        ("@errands"        . ?r)
                        ("@comp"           . ?c)
                        ("@home"           . ?h)
                        ("@work"           . ?w)
                        (:endgroup . nil)
                        (:startgrouptag . nil)
                        ("@work")
                        (:grouptags)
                        ("@anywhere")
                        ("@comp")
                        (:endgrouptag . nil)
                        ("@@someday_maybe" . ?s)
                        ("@@aspirational"  . ?z)
                        ("@@frontburner"   . ?f)
                        ("%quick"          . ?q)
                        ("%easy"           . ?e)))
  (setq org-startup-with-latex-preview t)
  (setq org-directory "~/.local/share/notes")
  (setq org-todo-keywords
        '((sequence "INCUBATE(i)" "READY(r/!)" "TODO(t)" "NEXT(n)" "WAIT(w@!/!)" "|" "DONE(d!)" "CNCL(c@!)")))
  (setq org-todo-keyword-faces
        '(("INCUBATE" . (:foreground "#dfaf8f" :weight bold))
          ("READY" . (:foreground "#8cd0d3" :weight bold))
          ("NEXT" . (:foreground "#f0dfaf" :weight bold))
          ("WAIT" . (:foreground "#dc8cc3" :weight bold))
          ("CNCL" . (:foreground "#d26478" :weight bold))))
  (setq org-log-into-drawer t)
  (setq org-log-done 'time))

(use-package! org-ql
  :commands (+patch/toggle-quick-agenda-filter
             +patch/toggle-easy-agenda-filter
             +patch-gtd/planning/daily-planning-layout
             +patch-gtd/planning/weekly-planning-layout
             +patch-gtd/planning/quarterly-planning-layout
             +patch-gtd/planning/yearly-planning-layout)
  :defines (+patch/toggle-quick-agenda-filter
            +patch/toggle-easy-agenda-filter
            +patch-gtd/planning/daily-planning-layout
            +patch-gtd/planning/weekly-planning-layout
            +patch-gtd/planning/quarterly-planning-layout
            +patch-gtd/planning/yearly-planning-layout)
  :init
  (map! (:map (evil-normal-state-map evil-org-agenda-mode-map org-super-agenda-header-map org-agenda-keymap)
              (:prefix-map ("DEL" . "GTD")
                           (:prefix ("v" . "Planning Views")
                            :desc "Yearly Planning"     "y" #'+patch-gtd/planning/yearly-planning-layout
                            :desc "Quarterly Planning"  "q" #'+patch-gtd/planning/quarterly-planning-layout
                            :desc "Quarterly Review"    "Q" #'+patch-gtd/planning/quarterly-review-layout
                            :desc "Weekly Planning"     "w" #'+patch-gtd/planning/weekly-planning-layout
                            ;; :desc "Refresh Weekly Data" "W" #'+patch/refresh-weekly-planning-view
                            :desc "Daily Planning"      "d" #'+patch-gtd/planning/daily-planning-layout
                            :desc "GTD File"            "g" #'+patch-gtd/planning/gtd-file-layout
                            :desc "Inbox"               "i" #'+patch-gtd/planning/inbox-layout)
                           (:prefix ("p" . "Planning Actions")
                            :desc "Mark as 'to-plan'"       "p" #'+patch-gtd/planning/move-to-planning-queue
                            :desc "Mark as READY"           "r" #'+patch-gtd/planning/move-ready
                            :desc "Open this quarter"       "o" #'+patch-gtd/planning/agenda-open-this-quarter-move
                            :desc "Punt to another quarter" "u" #'+patch-gtd/planning/agenda-punt-move
                            ))
              "<backspace>" nil
              :m "<backspace>" nil
              "<delete>" nil
              :m "<delete>" nil))
  ;; all-the-icons is necessary for some reason, just load it incrementally at startup
  ;; need to force ob-jupyter to load so we can start a jupyter kernel for generating the plots for the weekly view
  :defer-incrementally (all-the-icons ob-jupyter org-caldav org-jira)
  :config
  (defun +patch/bookmark-org-ql-view (org-ql-view-name)
    (bookmark-store
     (format "Org QL View: %s" org-ql-view-name)
     (list (cons 'org-ql-view-plist (alist-get org-ql-view-name org-ql-views nil nil #'string=))
           '(handler . org-ql-view-bookmark-handler))
     nil))
  
  ;; heavily inspired by the yequake code for setting up buffers
  (defun +patch/open-window-layout (buffer-refs-or-fns)
    "Show buffers or run functions in order defined in BUFFER-REFS-OR-FNS."
    (cl-flet ((open-buffer-or-call-fn (it) (cl-typecase it
                                             (string (or (get-buffer it)
                                                         (find-buffer-visiting it)
                                                         (find-file-noselect it)))
                                             (function (funcall it)))))
      (let ((split-width-threshold 0)
            (split-height-threshold 0))
        ;; Switch to first buffer, pop to the rest.
        (switch-to-buffer (open-buffer-or-call-fn (car buffer-refs-or-fns)))
        (dolist (buffer-ref-or-fn (cdr buffer-refs-or-fns))
          (when-let* ((ret (open-buffer-or-call-fn buffer-ref-or-fn)))
            (display-buffer-same-window ret nil))))))
  (defun +patch-dayone/clean-task ()
    (ignore-errors (org-priority 'remove))
    (ignore-errors (org-schedule '(4)))  ;; prefix arg to unschedule
    (ignore-errors (org-entry-delete (point) "OPENED")))
  
  (defun +patch-dayone/hatch (&optional pom)
    (interactive)
    (+patch-dayone/clean-task)
    (org-todo "TODO"))
  
  (defun +patch-dayone/incubate ()
    (interactive)
    (+patch-dayone/clean-task)
    (org-todo "READY"))
  
  (defun +patch-dayone/agenda/hatch ()
    (interactive)
    (+patch--from-source-of-agenda-entry (+patch-dayone/hatch)))
  
  (defun +patch-dayone/agenda/incubate ()
    (interactive)
    (+patch--from-source-of-agenda-entry (+patch-dayone/incubate)))
  
  (setq org-agenda-bulk-custom-functions
        (append org-agenda-bulk-custom-functions '((?i +patch-dayone/agenda/incubate)
                                                   (?h +patch-dayone/agenda/hatch))))
  (map! (:map org-agenda-mode-map "i" #'+patch-dayone/agenda/incubate)
        (:map org-agenda-mode-map "h" #'+patch-dayone/agenda/hatch)
        (:map org-agenda-keymap "i" #'+patch-dayone/agenda/incubate)
        (:map org-agenda-keymap "h" #'+patch-dayone/agenda/hatch)
        (:map evil-org-agenda-mode-map :m "i" #'+patch-dayone/agenda/incubate)
        (:map evil-org-agenda-mode-map :m "h" #'+patch-dayone/agenda/hatch))
  (defun +patch-gtd/planning/quarterly-planning-layout ()
    (interactive)
    (+patch-gtd/set-or-refresh-quarterly-views)
    (+patch/open-window-layout '(delete-other-windows
                               (lambda () (org-ql-view "Backburner"))
                               delete-other-windows
                               split-window-horizontally
                               (lambda () (enlarge-window (/ (frame-width) 10) t))
                               (lambda () (org-ql-view "Unopened Active Tasks"))
                               (lambda () (evil-window-right 1)))))
  
  
  (defun +patch/ts-quarter (ts)
    (let ((this-month (ts-month ts)))
      (cond ((< this-month 4) 1)
            ((< this-month 7) 2)
            ((< this-month 10) 3)
            (t 4))))
  
  (defun +patch/ts-quarter-with-year (ts)
    (format "%s-Q%s" (ts-year (ts)) (+patch/ts-quarter (ts))))
  
  (defun +patch-dayone/open (&optional pom)
    (interactive)
    ;; makes it so I can use this to demote tasks from the frontburner, as well as promote to the backburner
    (org-toggle-tag "@@frontburner" 'off)
    (when (not (org-entry-get nil "OPENED"))
      (+patch/set-opened-date (or pom (point)) (ts-format (ts-now)))))
  
  (defun +patch-dayone/open-this-quarter (&optional pom)
    (interactive)
    (org-entry-put pom "PLANNED-FOR-QUARTER" (+patch/ts-quarter-with-year (ts-now)))
    (+patch/set-opened-date (or pom (point)) (ts-format (+patch/start-of-this-quarter-ts))))
  
  (defun +patch-dayone/open-this-year (&optional pom)
    (interactive)
    (org-entry-put pom "PLANNED-FOR-YEAR" (number-to-string (ts-year (ts-now))))
    (org-entry-put pom "PLANNED-FOR-QUARTER" (+patch/ts-quarter-with-year (ts-now)))
    (+patch/set-opened-date (or pom (point)) (ts-format (+patch/start-of-this-year-ts))))
  
  (defun +patch-dayone/agenda/open ()
    (interactive)
    (+patch--from-source-of-agenda-entry (+patch-dayone/open)))
  
  (defun +patch-dayone/agenda/open-this-quarter ()
    (interactive)
    (+patch--from-source-of-agenda-entry (+patch-dayone/open-this-quarter)))
  
  (defun +patch-dayone/agenda/open-this-year ()
    (interactive)
    (+patch--from-source-of-agenda-entry (+patch-dayone/open-this-year)))
  
  (defun +patch-dayone/planning/open ()
    (interactive)
    (+patch--from-source-of-agenda-entry (+patch-dayone/open))
    (org-ql-view-refresh))
  
  (setq org-agenda-bulk-custom-functions
        (append org-agenda-bulk-custom-functions '((?o +patch-dayone/agenda/open))))
  (map! (:map (org-agenda-mode-map org-agenda-keymap) "o" #'+patch-dayone/agenda/open)
        (:map evil-org-agenda-mode-map :m "o" #'+patch-dayone/agenda/open)
        (:map evil-org-agenda-mode-map :m "O" nil)
        (:map org-super-agenda-header-map :m "O" nil)
        (:map (org-agenda-mode-map org-agenda-keymap evil-org-agenda-mode-map)
              (:prefix ("O" . "Open At Time")
               :desc "Now"              "n"   #'+patch-dayone/agenda/open
               :desc "For This Quarter" "q"   #'+patch-dayone/agenda/open-this-quarter
               :desc "For This Year"    "y"   #'+patch-dayone/agenda/open-this-year)))
  (defun +patch/generate-quarters-burnup-plot ()
    (interactive)
    (+patch/invoke-babel-named "~/.config/doom/modules/lang/org-patch/config.org" "quarters-tasks")
    (+patch/invoke-babel-named "~/.config/doom/modules/lang/org-patch/config.org" "plot-quarters-tasks"))
  (defun +patch/generate-years-burnup-plot ()
    (interactive)
    (+patch/invoke-babel-named "~/.config/doom/modules/lang/org-patch/config.org" "years-tasks")
    (+patch/invoke-babel-named "~/.config/doom/modules/lang/org-patch/config.org" "plot-years-tasks"))
  
  (defun +patch/generate-quarterly-velocity-plot ()
    (interactive)
    (+patch/invoke-babel-named "~/.config/doom/modules/lang/org-patch/config.org" "all-time-tasks")
    (+patch/invoke-babel-named "~/.config/doom/modules/lang/org-patch/config.org" "plot-quarterly-velocity"))
  
  (defun +patch-gtd/set-or-refresh-quarterly-review-views ()
    (+patch/set-orgql-view
     "Backburner Review"
     `(:buffers-files ("~/.local/share/notes/gtd/org-gtd-tasks.org")
       :query (and ,+patch-dayone/is-open ,+patch/is-action)
       :sort (priority todo)
       :narrow nil
       :super-groups ((:auto-outline-path t))
       :title ,(format "[Completed last quarter: %s] [Planned for this quarter: %s]" (+patch/num-tasks-completed-last-quarter) (+patch/num-tasks-planned-for-this-quarter)))))
  
  (defun +patch-gtd/planning/quarterly-review-layout ()
    (interactive)
    (after! ob-jupyter
      (org-babel-jupyter-aliases-from-kernelspecs))
    (+patch-gtd/set-or-refresh-quarterly-review-views)
    (+patch/generate-years-burnup-plot)
    (+patch/generate-quarterly-velocity-plot)
    (+patch/open-window-layout '(delete-other-windows
                                 (lambda () (org-ql-view "Backburner Review"))
                                 delete-other-windows  ;; bc org-ql does weird things with opening windows...
                                 split-window-horizontally
                                 "~/.config/.../.config/doom/modules/lang/org-patch/years-burnup.png"
                                 split-window-vertically
                                 "~/.config/.../.config/doom/modules/lang/org-patch/quarterly-velocity.png"
                                 (lambda () (evil-window-right 1))
                                 (lambda () (enlarge-window (/ (frame-width) 10) t))
                                 )))
  
  
  (defun +patch-gtd/planning/weekly-planning-layout ()
    (interactive)
    (after! ob-jupyter
      (org-babel-jupyter-aliases-from-kernelspecs))
    (+patch-gtd/set-or-refresh-weekly-views)
    (+patch/generate-quarters-burnup-plot)
    (+patch/open-window-layout '(delete-other-windows
                                 (lambda () (org-ql-view "This Week's Agenda"))
                                 delete-other-windows
                                 split-window-horizontally
                                 (lambda () (org-ql-view "Non-Scheduled Backburner"))
                                 (lambda () (evil-window-right 1))
                                 (lambda () (enlarge-window (/ (frame-width) 10) t))
                                 (lambda () (evil-window-left 1))
                                 +patch-dayone/hide-all-org-ql-groups
                                 ;; other-window
                                 split-window-vertically
                                 (lambda () (evil-window-down 1))
                                 "~/.config/.../.config/doom/modules/lang/org-patch/quarters-burnup.png"
                                 ;; I think this consult call is to update the plot, but iirc it isn't working anyway
                                 ;; (lambda () (funcall consult--buffer-display "quarters-burnup.png"))
                                 (lambda () (evil-window-up 1))
                                 ;; "quarters-burnup.png"
                                 ;; "*Org QL View: Weekly Planning*"
                                 (lambda () (evil-window-right 1))
                                 ;; (lambda () (funcall consult--buffer-display "*Org QL View: This Week's Agenda*"))
                                 )))
  (defun +patch-dayone/send-to-frontburner ()
    (interactive)
    (org-schedule '(4))
    (org-set-tags "@@frontburner"))
  
  (defun +patch-dayone/agenda/send-to-frontburner ()
    (interactive)
    (org-agenda-schedule '(4))
    (org-agenda-set-tags "@@frontburner"))
  
  (setq org-agenda-bulk-custom-functions
        (append org-agenda-bulk-custom-functions '((?F +patch-dayone/agenda/send-to-frontburner))))
  (map! (:map org-agenda-mode-map "f" #'+patch-dayone/agenda/send-to-frontburner)
        (:map org-agenda-keymap "f" #'+patch-dayone/agenda/send-to-frontburner)
        (:map evil-org-agenda-mode-map :m "f" #'+patch-dayone/agenda/send-to-frontburner))
  (defun +patch/is-substr (comparison-string query-string)
    (string-match-p (regexp-quote query-string) comparison-string))
  
  (defun +patch/agenda-filter-already-applied (tag-name)
    (seq-contains-p org-agenda-tag-filter tag-name #'+patch/is-substr))
  
  (defun +patch/remove-match-from-seq (query-string seq)
    (seq-remove (lambda (elt) (+patch/is-substr elt query-string)) seq))
  
  (defun +patch/remove-agenda-tag-filter (tag-name)
    (setq org-agenda-tag-filter (+patch/remove-match-from-seq tag-name org-agenda-tag-filter))
    (org-agenda-redo))
  
  (defun +patch/set-quick-agenda-tag ()
    "Annoyingly, I have to do this manually since org-agenda-filter-by-tag accepts ?q as a 'quit' argument."
    (setq org-agenda-tag-filter
          (cons "+%quick" org-agenda-tag-filter))
    (org-agenda-filter-apply org-agenda-tag-filter 'tag))
  
  (defun +patch/toggle-quick-agenda-filter ()
    (interactive)
    (if (+patch/agenda-filter-already-applied "%quick")
        (+patch/remove-agenda-tag-filter "%quick")
      (+patch/set-quick-agenda-tag)))
  
  (defun +patch/toggle-easy-agenda-filter ()
    (interactive)
    (if (+patch/agenda-filter-already-applied "%easy")
        (+patch/remove-agenda-tag-filter "%easy")
      (org-agenda-filter-by-tag '(16) ?e))) ; prefix arg to accumulate tags (rather than just replacing)
  
  (map! (:map evil-motion-state-map
         :desc "temporarily delete kbd to avoid 'non-prefix key' error, also this always annoyed me anyway" "-" nil)
        (:map (evil-org-agenda-mode-map org-super-agenda-header-map org-agenda-keymap)
         :desc "temporarily delete kbd to avoid 'non-prefix key' error" "-" nil
         :desc "temporarily delete kbd to avoid 'non-prefix key' error" :m "-" nil
         (:prefix ("-" . "Filter Agenda")
          :desc "Toggle %quick filter"  "q"   #'+patch/toggle-quick-agenda-filter
          :desc "Toggle %easy filter"   "e"   #'+patch/toggle-easy-agenda-filter
          :desc "Filter by action list" "a"   #'org-agenda-filter
          :desc "Clear filters"         "DEL" #'org-agenda-filter-remove-all)))
  (defun +patch-gtd/planning/daily-planning-layout ()
    (interactive)
    ;; sometimes emacs seems to think we're in a side window when we're not, but I can't figure out why so just ignore the error.
    (+patch/open-window-layout '((lambda () (ignore-errors delete-other-windows))
                                 (lambda () (org-agenda nil ","))
                                 delete-other-windows))
      (save-excursion
        (org-jira-get-issues (org-jira-get-issue-list)))
    )
  (defun +patch-gtd/planning/gtd-file-layout ()
    (interactive)
    ;; sometimes emacs seems to think we're in a side window when we're not, but I can't figure out why so just ignore the error.
    (+patch/open-window-layout '((lambda () (ignore-errors delete-other-windows))
                                 "~/.local/share/notes/gtd/org-gtd-tasks.org"
                                 delete-other-windows)))
  (defun +patch-gtd/planning/inbox-layout ()
    (interactive)
    (+patch/open-window-layout '((lambda () (ignore-errors delete-other-windows))
                                 "~/.local/share/notes/gtd/inbox.org"
                                 delete-other-windows)))
  )

(after! ts
  (after! org-ql

    (defun +patch/org-element-contents (element)
      "Get the contents of the partially specified 'element' that only consists of '(TYPE PROPS)'."
      (let ((beg (org-element-property :contents-begin element))
            (end (org-element-property :contents-end element)))
        (buffer-substring-no-properties beg end)))

    (defun +patch/maybe-parse-element-date (prop-name task)
      (let ((value (org-element-property prop-name task)))
        (when value
          (ts-format "%Y-%m-%d" (ts-parse-org-element value)))))

    (defun +patch/find-and-parse-task ()
      (+patch/parse-task (org-element-at-point)))

    (defun +patch/parse-task (raw-task)
      (when raw-task
        `(,(org-element-property :raw-value raw-task)
          ,(let ((todo-keyword (org-element-property :todo-keyword raw-task)))
             (when todo-keyword (prin1-to-string (read todo-keyword))))
          ,(prin1-to-string (org-element-property :todo-type raw-task))
          ,(+patch/maybe-parse-element-date :closed raw-task)
          ,(+patch/maybe-parse-element-date :scheduled raw-task)
          ,(+patch/get-opened-date raw-task)
          ,(org-element-property :PLANNED-FOR-YEAR raw-task)
          ,(org-element-property :PLANNED-FOR-QUARTER raw-task))))

    (defun +patch-dayone/this-quarters-tasks (&optional as-of)
      (org-ql-query
        :select #'+patch/find-and-parse-task
        :from (cons "~/.local/share/notes/gtd/org-gtd-tasks.org" (f-glob "gtd_archive_[0-9][0-9][0-9][0-9]" "~/.local/share/notes/gtd"))
        :where +patch-dayone/planned-for-this-quarter))

    (defun +patch-dayone/this-years-tasks (&optional as-of)
      (org-ql-query
        :select #'+patch/find-and-parse-task
        :from (cons "~/.local/share/notes/gtd/org-gtd-tasks.org" (f-glob "gtd_archive_[0-9][0-9][0-9][0-9]" "~/.local/share/notes/gtd"))
        :where +patch-dayone/planned-for-this-year))

    (defun +patch-dayone/all-time-tasks (&optional as-of)
      (org-ql-query
        :select #'+patch/find-and-parse-task
        :from (cons "~/.local/share/notes/gtd/org-gtd-tasks.org" (f-glob "gtd_archive_[0-9][0-9][0-9][0-9]" "~/.local/share/notes/gtd"))
        :where +patch-dayone/has-been-open))))

(use-package! origami
  :after (org-agenda evil-org-agenda org-super-agenda)
  :hook ((org-agenda-mode . origami-mode)
         (org-agenda-finalize . +patch/org-super-agenda-origami-fold-default))
  :config
  (setq +patch/agenda-auto-hide-groups '("Waiting" "Completed Today" "Could Pull In" "Jira" "Frontburner"))
  (defun +patch/org-super-agenda-origami-fold-default ()
    "Fold certain groups by default in Org Super Agenda buffer."
    (evil-goto-first-line)

    (--each +patch/agenda-auto-hide-groups
      (goto-char (point-min))
      (when (re-search-forward (rx-to-string `(seq bol " " ,it)) nil t)
        (origami-close-node (current-buffer) (point))))

    (beginning-of-buffer))

  (defun +patch/dont-show-waiting-in-agenda ()
    (interactive)
    (setq +patch/agenda-auto-hide-groups
          (cons "Waiting" +patch/agenda-auto-show-groups))
    (org-agenda-redo))

  (defun +patch/show-waiting-in-agenda ()
    (interactive)
    (setq +patch/agenda-hide-show-groups
          (remove "Waiting" +patch/agenda-auto-show-groups))
    (org-agenda-redo))

  (defun +patch-dayone/hide-all-org-ql-groups ()
    (evil-goto-first-line)
    (forward-line 1)
    (cl-loop do (origami-forward-toggle-node (current-buffer) (point))
             while (origami-forward-fold-same-level (current-buffer) (point)))
    (evil-goto-first-line))

  (map!
   (:map evil-org-agenda-mode-map "TAB" #'origami-toggle-node)
   (:map evil-org-agenda-mode-map :m "<tab>" #'origami-toggle-node)
   (:map evil-org-agenda-mode-map :m "TAB" #'origami-toggle-node)
   (:map org-super-agenda-header-map :m "<tab>" #'origami-toggle-node)
   (:map org-super-agenda-header-map :m "TAB" #'origami-toggle-node)
   (:map org-super-agenda-header-map "TAB" #'origami-toggle-node)
   (:map org-agenda-keymap "TAB" #'origami-toggle-node)
   (:map org-agenda-keymap "<tab>" #'origami-toggle-node)
   (:map org-agenda-mode-map "TAB" #'origami-toggle-node)
   (:map org-agenda-mode-map "<tab>" #'origami-toggle-node)
   :map org-agenda-mode-map
   :localleader
   ("w" #'+patch/show-waiting-in-agenda)
   ("W" #'+patch/dont-show-waiting-in-agenda)))

(use-package! transient
  :after ts
  :config
  (defclass transient-preset (transient-infix)
    ((transient                            :initform t)
     (format      :initarg :format         :initform " %k %d %v")
     (arguments   :initarg :arguments))
    "Class used for command-line arguments presets.")
  
  (cl-defmethod transient-init-value ((obj transient-preset))
    "if default values match the arguments slot of transient_preset then set its init value to t"
    (oset obj value
          (seq-set-equal-p (oref obj arguments)
                           (oref transient--prefix value))))
  
  (cl-defmethod transient-infix-read ((obj transient-preset))
    "Toggle the preset on or off setting all the arguments to corresponding infixes."
    (pp (transient-args transient-current-command))
    (seq-set-equal-p (oref obj arguments)
                     (transient-args transient-current-command)))
  
  (cl-defmethod transient-infix-set ((obj transient-preset) value)
    "Toggle the preset on or off setting all the arguments to corresponding infixes."
    (oset obj value value)
    (unless value
      (oset transient--prefix value (oref obj arguments))
      (mapc #'transient-init-value transient--suffixes)))
  
  (cl-defmethod transient-format-value ((obj transient-preset))
    (propertize
     (concat "[" (mapconcat 'identity (oref obj arguments) " ") "]")
     'face (if (oref obj value)
               'transient-argument
             'transient-inactive-argument)))
  
  (cl-defmethod transient-infix-value ((_ transient-preset))
    "Return nil, which means \"no value\"."
    nil)
  
  (after! org
    (defun +patch-gtd/set-task-attrs (&optional todo tags opened-date scheduled-date rfloc goto-task)
      "For any args provided, apply the value to the appropriate field of the task
    at point.
    
    If GOTO-TASK is specified, narrow to the task after applying properties so the
    user can make any appropriate edits."
      (when todo (org-todo todo))
      (when tags (org-set-tags tags))
      (when opened-date (+patch/set-opened-date nil opened-date))
      (when scheduled-date (org-schedule nil scheduled-date))
      (when rfloc (org-refile nil nil rfloc))
      (when goto-task
        (push-mark (point))
        (org-refile '(16) nil rfloc)
        (org-tree-to-indirect-buffer)
        (+patch/refine-project-mode t)
        (push (lambda () (pop-global-mark) (pop +patch--widen-hooks)) +patch--widen-hooks)))
    (defun +patch--get-tags ()
      "Get tags from user using 'org-fast-tag-selection'. Mostly taken from the
    implementation of 'org-set-tags-command'."
      (let* ((all-tags (org-get-tags))
             (local-table (or org-current-tag-alist (org-get-buffer-tags)))
             (table (setq org-last-tags-completion-table
                          (append
                           ;; Put local tags in front.
                           local-table
                           (cl-set-difference
                            (org--tag-add-to-alist
                             (and org-complete-tags-always-offer-all-agenda-tags
                                  (org-global-tags-completion-table
                                   (org-agenda-files)))
                             local-table)
                            local-table))))
             (current-tags
              (cl-remove-if (lambda (tag) (get-text-property 0 'inherited tag))
                            all-tags))
             (inherited-tags
              (cl-remove-if-not (lambda (tag) (get-text-property 0 'inherited tag))
                                all-tags)))
    
        (org-fast-tag-selection
         current-tags
         inherited-tags
         table
         (and org-fast-tag-selection-include-todo org-todo-key-alist))))
    
    (transient-define-infix +patch-gtd/planning/tag-infix ()
      :description "Tags"
      :class 'transient-option
      :always-read t
      :shortarg "-q"
      :argument "--tags="
      :reader (lambda (prompt _default-location _history)
                (+patch--get-tags)))
    
    (transient-define-infix +patch-gtd/planning/refile-infix ()
      :description "Refile Location"
      :class 'transient-option
      :always-read t
      :shortarg "-r"
      :argument "--refile="
      :reader (lambda (prompt _default-location _history)
                (when (fboundp 'org-refile-get-location)
                  (let* ((rfloc (org-refile-get-location "Where should the task be filed?"))
                         (rfloc-text (car rfloc)))
                    ;; NOTE: Store full rfloc as property of the text, so that
                    ;;       transient can display and we can still retrieve the
                    ;;       full object later.
                    (propertize rfloc-text 'rfloc rfloc)))))
    
    (transient-define-infix +patch-gtd/planning/todo-infix ()
      :description "Todo State"
      :class 'transient-option
      :always-read t
      :shortarg "-t"
      :argument "--todo="
      :reader (lambda (prompt _default-location _history)
                (when (fboundp 'org-fast-todo-selection)
                  (org-fast-todo-selection))))
    
    (transient-define-infix +patch-gtd/planning/opened-date-infix ()
      :description "Opened Date"
      :class 'transient-option
      :always-read t
      :shortarg "-o"
      :argument "--opened-date="
      :reader 'transient-read-date)
    
    (transient-define-infix +patch-gtd/planning/scheduled-date-infix ()
      :description "Scheduled Date"
      :class 'transient-option
      :always-read t
      :shortarg "-s"
      :argument "--scheduled-date="
      :reader 'transient-read-date)
    
    ;; "presets" that set typical combinations of infix args
    (transient-define-infix +patch-dayone/planning/someday-maybe-preset ()
      :class transient-preset
      :arguments '("--todo=READY"))
    
    (transient-define-infix +patch-dayone/planning/active-task-preset ()
      :class transient-preset
      :arguments '("--todo=TODO"))
    
    (transient-define-infix +patch-dayone/planning/backburner-preset ()
      :class transient-preset
      :arguments `("--todo=TODO"
                   ,(format "--opened-date=%s" (org-read-date nil nil "today"))))
    
    (transient-define-infix +patch-dayone/planning/frontburner-preset ()
      :class transient-preset
      :arguments `("--todo=NEXT"
                   ,(format "--opened-date=%s"
                            (ts-format "%Y-%m-%d" (ts-apply :hour 0 :minute 0 :second 0 (ts-now))))
                   "--tags=@@frontburner"))
    
    (transient-define-suffix +patch-gtd/apply-task-attrs ()
      "Set whichever attributes are necessary for the task being processed."
      :transient nil
      (interactive)
      (let* ((urgency (transient-arg-value "--urgency=" (transient-args '+patch-gtd/process-inbox-item)))
             (todo (transient-arg-value "--todo=" (transient-args '+patch-gtd/process-inbox-item)))
             (rfloc (let ((rfloc-text (transient-arg-value "--refile=" (transient-args '+patch-gtd/process-inbox-item))))
                      (if rfloc-text
                          (get-text-property 0 'rfloc rfloc-text)
                        nil)))
             (tags (transient-arg-value "--tags=" (transient-args '+patch-gtd/process-inbox-item)))
             (opened-date (transient-arg-value "--opened-date=" (transient-args '+patch-gtd/process-inbox-item)))
             (scheduled-date (transient-arg-value "--scheduled-date=" (transient-args '+patch-gtd/process-inbox-item))))
        (when (and (not scheduled-date) (equal urgency "to-schedule"))
          (setq scheduled-date (org-read-date)))
        (+patch-gtd/set-task-attrs todo tags opened-date scheduled-date rfloc)))
    
    (transient-define-suffix +patch-gtd/apply-task-attrs-and-goto-task ()
      "Set whichever attributes are necessary for the task being processed, then
    narrow to the task to make any appropriate edits."
      :transient nil
      (interactive)
      (let* ((urgency (transient-arg-value "--urgency=" (transient-args '+patch-gtd/process-inbox-item)))
             (todo (transient-arg-value "--todo=" (transient-args '+patch-gtd/process-inbox-item)))
             (rfloc (let ((rfloc-text (transient-arg-value "--refile=" (transient-args '+patch-gtd/process-inbox-item))))
                      (if rfloc-text
                          (get-text-property 0 'rfloc rfloc-text)
                        nil)))
             (tags (transient-arg-value "--tags=" (transient-args '+patch-gtd/process-inbox-item)))
             (opened-date (transient-arg-value "--opened-date=" (transient-args '+patch-gtd/process-inbox-item)))
             (scheduled-date (transient-arg-value "--scheduled-date=" (transient-args '+patch-gtd/process-inbox-item))))
        (+patch-gtd/set-task-attrs todo tags opened-date scheduled-date rfloc t)))
    
    
    (transient-define-prefix +patch-gtd/process-inbox-item ()
      "Process a task from the inbox, setting whichever task attributes are
    necessary for the particular task."
      :value '("--todo=READY")
      ["Presets"
       ("s" "Someday/Maybe" +patch-dayone/planning/someday-maybe-preset)
       ("a" "Active Tasks" +patch-dayone/planning/active-task-preset)
       ("b" "Backburner" +patch-dayone/planning/backburner-preset)
       ("f" "Frontburner" +patch-dayone/planning/frontburner-preset)]
      ["Task Attributes"
       (+patch-gtd/planning/opened-date-infix)
       (+patch-gtd/planning/scheduled-date-infix)
       (+patch-gtd/planning/refile-infix)
       (+patch-gtd/planning/todo-infix)
       (+patch-gtd/planning/tag-infix)
       ]
      ["Apply"
       ("x" "apply and eXit" +patch-gtd/apply-task-attrs)
       ("d" "apply and eDit" +patch-gtd/apply-task-attrs-and-goto-task)])))

(after! bufler
  (setq bufler-groups (bufler-defgroups
                        (group
                         ;; Subgroup collecting all named workspaces.
                         (auto-workspace))
                        (group
                         ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
                         (group-or "*Help/Info*"
                                   (mode-match "*Help*" (rx bos "help-"))
                                   (mode-match "*Info*" (rx bos "info-"))))
                        (group
                         ;; Subgroup collecting all special buffers (i.e. ones that are not file-backed),
                         ;; except certain ones like Dired, Forge, or Magit buffers (which are allowed to
                         ;; fall through to other groups, so they end up grouped with their project buffers).
                         (group-not "*Special"
                                    (group-or "*Special*"
                                              (mode-match "Magit" (rx bos "magit-"))
                                              (mode-match "Forge" (rx bos "forge-"))
                                              (mode-match "Dired" (rx bos "dired"))
                                              (mode-match "grep" (rx bos "grep-"))
                                              (mode-match "compilation" (rx bos "compilation-"))
                                              (auto-file)))
                         (group
                          ;; Subgroup collecting these "special special" buffers
                          ;; separately for convenience.
                          (name-match "**Special**"
                                      (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
                         (group
                          ;; Subgroup collecting all other Magit buffers, grouped by directory.
                          (mode-match "*Magit* (non-status)" (rx bos "magit-"))
                          (auto-directory))
                         ;; Subgroup for Helm buffers.
                         (mode-match "*Helm*" (rx bos "helm-"))
                         ;; Remaining special buffers are grouped automatically by mode.
                         (auto-mode))
                        (group-or
                         "gtd"
                         (name-match "Org QL" (rx bos "*Org QL View:"))
                                        ;(name-match "Burnup Chart" (rx eos "-burnup*"))
                         (dir "~/.local/share/notes/gtd"))
                        (group-or
                         "notes"
                         (dir "~/.local/share/notes"))
                        (group-or
                         "..."
                         (dir "~/.config/..."))
                        ;; All buffers under "~/.emacs.d" (or wherever it is).
                        (dir user-emacs-directory)
                        (group
                         ;; Subgroup collecting buffers in `org-directory' (or "~/org" if
                         ;; `org-directory' is not yet defined).
                         (dir (if (bound-and-true-p org-directory)
                                  org-directory
                                "~/org"))
                         (group
                          ;; Subgroup collecting indirect Org buffers, grouping them by file.
                          ;; This is very useful when used with `org-tree-to-indirect-buffer'.
                          (auto-indirect)
                          (auto-file))
                         ;; Group remaining buffers by whether they're file backed, then by mode.
                         (group-not "*special*" (auto-file))
                         (auto-mode))
                        (group
                         ;; Subgroup collecting buffers in a projectile project.
                         (auto-projectile)
                         (group-not "special"
                                    ;; This subgroup collects special buffers so they are
                                    ;; easily distinguished from file buffers.
                                    (group-or "Non-file-backed and neither Dired nor Magit"
                                              (mode-match "Magit Status" (rx bos "magit-status"))
                                              (mode-match "Dired" (rx bos "dired-"))
                                              (auto-file))))
                        (group
                         ;; Subgroup collecting buffers in a version-control project,
                         ;; grouping them by directory (using the parent project keeps,
                         ;; e.g. git worktrees with their parent repos).
                         (auto-parent-project)
                         (group-not "special"
                                    ;; This subgroup collects special buffers so they are
                                    ;; easily distinguished from file buffers.
                                    (group-or "Non-file-backed and neither Dired nor Magit"
                                              (mode-match "Magit Status" (rx bos "magit-status"))
                                              (mode-match "Dired" (rx bos "dired-"))
                                              (auto-file))))
                        ;; Group remaining buffers by directory, then major mode.
                        (auto-directory)
                        (auto-mode))))

(use-package! org-jira
  :custom
  (org-jira-working-dir "~/.local/share/notes/gtd/jira")
  (jiralib-url "https://humansignal.atlassian.net")
  (jiralib-user "patrick@humansignal.com")
  (org-jira-default-jql "project = \"DIA\" AND Sprint in openSprints() AND (assignee = currentUser() OR status IN (\"In Review\", Confirmed)) ORDER BY  priority DESC, created ASC")
  (org-jira-jira-status-to-org-keyword-alist
      '(("Ready for Dev" . "READY")
        ("In Progress" . "TODO")
        ("In Review" . "REVIEW")
        ("Confirmed" . "CONFIRMED")))
  :config
  (add-to-list 'org-agenda-files org-jira-working-dir)
  (defconst org-jira-progress-issue-flow
  '(("To Refine" . "To Groom")
    ("To Groom" . "Ready For Dev")
    ("Ready for Dev" . "In Progress")
    ("In Progress" . "In Review")
    ("In Review" . "QA")
    ("QA" . "Confirmed")
    ("Confirmed" . "Delivered")))
  (after! code-review
    (defun +patch/get-pr-url-for-ticket (jira-key)
      "Find ticket identified by JIRA-KEY, and return an associated PR url.
  
  If the ticket has no PRs associated, return nil.
  If the ticket has exactly one PR associated, return the associated url.
  If the ticket has many PRs, ask the user which one to return the URL for."
      (let* ((issue-id (alist-get 'id (jiralib-get-issue jira-key)))
             (dev-info (jiralib--rest-call-it (format "/rest/dev-status/latest/issue/summary?issueId=%s" issue-id)))
             (pr-info (alist-get 'pullrequest (alist-get 'summary dev-info)))
             (pr-count (alist-get 'count (alist-get 'overall pr-info))))
        (if (> pr-count 0)
            (let* ((application-type (alist-get 'name (cdar (alist-get 'byInstanceType pr-info))))
                   (detail-info (jiralib--rest-call-it (format "/rest/dev-status/latest/issue/detail?issueId=%s&applicationType=%s&dataType=%s" issue-id application-type 'pullrequest)))
                   (pr-detail-info (alist-get 'pullRequests
                                              (elt (alist-get 'detail detail-info) 0))))
              (if (> (length pr-detail-info) 1)
                  ;; if there are multiple prs, let the user choose
                  (let* ((choices (seq-map (lambda (elem) `(,(format "%s/%s" (alist-get 'repositoryName elem) (alist-get 'branch (alist-get 'source elem)))
                                                            . ,(alist-get 'url elem)))
                                           pr-detail-info))
                         (choice-name (completing-read "Choose a PR to review: " choices))
                         (chosen-obj (assoc choice-name choices))
                         (chosen-url (cdr chosen-obj)))
                    chosen-url)
                ;; if there's only 1 pr, just return that one
                (alist-get 'url (elt pr-detail-info 0))))
          ;; if there are no prs, return nil
          nil)))
  
    (defun +patch/start-review-for-ticket (jira-key)
      "Open a PR associated with ticket JIRA-KEY in `code-review`, if one exists."
      (let ((pr-url (+patch/get-pr-url-for-ticket jira-key)))
        (if pr-url
            (code-review-start pr-url)
          (message (format "No PR found for ticket %s" jira-key)))))
  
    (defun +patch/agenda/get-jira-key ()
      "Find the jira key for the entry at point in the agenda."
      (+patch--from-source-of-agenda-entry
       (org-entry-get (point) "ID")))
  
    (defun +patch/agenda/start-review-for-ticket-at-point ()
      "Open the PR for the ticket for the entry at point, if one exists."
      (interactive)
      (+patch/start-review-for-ticket (+patch/agenda/get-jira-key)))
  
    (map! (:map (org-agenda-keymap org-agenda-mode-map)
                "R" #'+patch/agenda/start-review-for-ticket-at-point))))

(use-package! org-caldav
  :after oauth2
  :custom
  (org-caldav-oauth2-client-id (plist-get (car (auth-source-search :host "org-caldav")) :clientid))
  (org-caldav-oauth2-client-secret (auth-source-pick-first-password :host "org-caldav"))
  (org-caldav-url 'google)
  (org-caldav-calendar-id (plist-get (car (auth-source-search :host "family")) :email))
  (org-caldav-inbox "~/.local/share/notes/calendar.org")
  (org-caldav-files '("~/.local/share/notes/gtd/org-gtd-tasks.org"))
  (org-icalendar-timezone "America/Los_Angeles")
  ;; keep from constantly asking for plist password
  (plstore-cache-passphrase-for-symmetric-encryption t)
  ;; following values taken from this GH issue: https://github.com/dengste/org-caldav/issues/267#issuecomment-1412135274
  ;; need to figure out what's appropriate, but this fixed pushing tasks to gcal
  (org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo))
  (org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due))
  ;; don't want my routine tasks cluttering my calendar
  (org-caldav-exclude-tags '("routine"))
  ;; don't prompt me to delete things from the cal (especially annoying for recurring tasks)
  (org-caldav-delete-calendar-entries "always")
  :config
  (add-to-list 'org-agenda-files org-caldav-inbox)
  ;; sync every 1 hour
  ;; TODO need to keep this from adding again if the timer already exists
  (run-with-timer 0 (* 60 (* 60 1)) #'org-caldav-sync))

(after! org
  (setq org-todo-repeat-to-state "TODO"))

(after! org
  (defun +patch/org-checkbox-todo ()
    "Switch header TODO state to DONE when all checkboxes are ticked, to TODO otherwise"
    (let ((todo-state (org-get-todo-state)) beg end)
      (unless (not todo-state)
        (save-excursion
          (org-back-to-heading t)
          (setq beg (point))
          (end-of-line)
          (setq end (point))
          (goto-char beg)
          (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                                 end t)
              (if (match-end 1)
                  (if (equal (match-string 1) "100%")
                      (unless (string-equal todo-state "DONE")
                        (org-todo 'done))
                    (unless (string-equal todo-state "TODO")
                      (org-todo 'todo)
                      (org-reset-checkbox-state-subtree)))
                (if (and (> (match-end 2) (match-beginning 2))
                         (equal (match-string 2) (match-string 3)))
                    (unless (string-equal todo-state "DONE")
                      (org-todo 'done)
                      (org-reset-checkbox-state-subtree))
                  (unless (string-equal todo-state "TODO")
                    (org-todo 'todo)))))))))

  (add-hook 'org-checkbox-statistics-hook '+patch/org-checkbox-todo))

(use-package! org-noter)
