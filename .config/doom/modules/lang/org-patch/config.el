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
  
  (defun +patch/org-agenda-refile (file headline)
    "Refile item at point to a particular place via org-agenda-refile, but
   with a simpler interface.
  
   FILE is the file to refile into.
  
   HEADLINE is the headline (inside FILE) to refile into."
    (save-window-excursion
      (org-agenda-refile nil (+patch/gen-org-refile-rfloc file headline))))
  
  ;; FIXME setting here instead of in :custom becuase it's not working in :custom (see note above)
  (setq +patch/org-gtd-tasks-file (concat (file-name-as-directory org-gtd-directory) "org-gtd-tasks.org"))
  
  (defun org-agenda-incubate (&optional arg)
    "Incubate a specified task (includes refiling to incubate section, and specifiying a date to review the task)"
    (interactive "P")
    (org-agenda-schedule arg)
    (+patch/org-agenda-refile +patch/org-gtd-tasks-file "Incubate"))
  
  (defun org-agenda-hatch (&optional arg)
    "Un-incubate (or 'hatch') a specified task (includes refiling to calendar section, and specifiying the date to complete the task)"
    (interactive "P")
    (org-agenda-schedule arg)
    (+patch/org-agenda-refile +patch/org-gtd-tasks-file "Calendar"))
  
  (setq org-agenda-bulk-custom-functions
        (append org-agenda-bulk-custom-functions '((?i org-agenda-incubate)
                                                   (?h org-agenda-hatch)
                                                   (?] org-planning-hatch))))
  (map! (:map org-agenda-mode-map "i" #'org-agenda-incubate)
        (:map org-agenda-mode-map "h" #'org-agenda-hatch)
        (:map org-agenda-keymap "h" #'org-agenda-hatch)
        (:map org-agenda-keymap "]" #'org-planning-hatch)
        (:map evil-org-agenda-mode-map "h" #'org-agenda-hatch)
        (:map evil-org-agenda-mode-map "]" #'org-planning-hatch)
        (:map evil-org-agenda-mode-map :m "i" #'org-agenda-incubate)
        (:map evil-org-agenda-mode-map :m "h" #'org-agenda-hatch)
        (:map evil-org-agenda-mode-map :m "]" #'org-planning-hatch))
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
               :desc "Views"               "v" #'org-ql-view
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

(use-package! doct
  :after (org org-capture)
  :commands (doct +patch/doct-properties)
  :defines +patch/doct-properties
  :custom
  ;; setq
  (org-capture-templates
   (append org-capture-templates
           (doct '(("Inbox"
                    :keys "i"
                    :file "~/.local/share/notes/gtd/inbox.org"
                    :template "* %?"
                    :kill-buffer t)
                   ("Email"
                    :keys "e"
                    :file "~/.local/share/notes/gtd/org-gtd-tasks.org"
                    :olp ("Email")
                    :template ("* TODO Reply: %a")
                    :kill-buffer t)
                   ("Today"
                    :keys "2"
                    :file "~/.local/share/notes/gtd/org-gtd-tasks.org"
                    :olp ("Calendar")
                    :hook +patch/doct-properties
                    ;; NOTE: Timestamp needs to be inactive (using the third arg
                    ;;       of org-insert-time-stamp) to avoid the OPENED date
                    ;;       appearing in the agenda.
                    :properties (:OPENED "%(org-insert-time-stamp (org-read-date nil t \"+0d\") nil t)")
                    :template ("* TODO %?"
                               "SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))")
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
                      :keys "f"
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
  :config
  (defun +patch/doct-properties ()
    "Add declaration's :properties to current entry."
    (let ((properties (doct-get :properties)))
      (dolist (keyword (seq-filter #'keywordp properties))
        (org-set-property (substring (symbol-name keyword) 1)
                          (replace-regexp-in-string "\n$" ""
                                                    (org-capture-fill-template (plist-get properties keyword)))))))
  ;; Usage:
  ;; (doct '(("My capture template"
  ;;          ...
  ;;          :hook +patch/org-property-drawer
  ;;          :properties (:anki_deck "${category}"))))
  )

(after! emacs-everywhere
  (defun get-app-name ()
    "Get the name of the current app (useful for returning to that app later). Currently uses osascript, so only useful on macos."
    (let ((default-directory emacs-everywhere--dir))
      (with-temp-buffer
        (call-process "osascript" nil t nil "app-name")
        (string-trim (buffer-string)))))

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
    (when emacs-everywhere-window-focus-command
      (apply #'call-process (car emacs-everywhere-window-focus-command)
             nil nil nil
             (mapcar (lambda (arg)
                       (replace-regexp-in-string "%w" (frame-parameter nil 'emacs-everywhere-prior-app) arg))
                     (cdr emacs-everywhere-window-focus-command))))
    (delete-frame)))

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
  (run-with-idle-timer 300 t (lambda () (save-window-excursion (org-agenda nil ","))))
  (defun org-agenda-reschedule-to-today (&optional arg)
    "Reschedule selected task(s) for today."
    (interactive "P")
    (org-agenda-schedule arg "."))
  
  (defun org-agenda-reschedule-to-tomorrow (&optional arg)
    "Reschedule selected task(s) for tomorrow."
    (interactive "P")
    (org-agenda-schedule arg "+1d"))
  
  (setq org-agenda-bulk-custom-functions '((?. org-agenda-reschedule-to-today)
                                           (?> org-agenda-reschedule-to-tomorrow)))
  (map! (:map org-agenda-mode-map "." #'org-agenda-reschedule-to-today)
        (:map evil-org-agenda-mode-map :m "." #'org-agenda-reschedule-to-today)
        (:map org-agenda-mode-map ">" #'org-agenda-reschedule-to-tomorrow)
        (:map evil-org-agenda-mode-map :m ">" #'org-agenda-reschedule-to-tomorrow)))

(use-package! org-refile
  :after org-agenda
  :config
  (add-to-list 'org-refile-targets `(,(directory-files "~/.local/share/notes/reference" t ".*\\.org$") :maxlevel . 3))
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
       (org-ql-block '(and (scheduled :on +0)
                           (not (children)) ; only look at actions, not projects
                           (not (todo "DONE" "CNCL" "WAIT" "INCUBATE"))
                           (regexp ,org-ql-regexp-scheduled-without-time))
                     ((org-ql-block-header "\n Today")))
       (org-ql-block '(and (scheduled
                            ;; :from ,(->> (ts-now)
                            ;;             (ts-adjust 'day (- (ts-dow (ts-now))))
                            ;;             (ts-apply :hour 0 :minute 0 :second 0))
                            :to -1)
                           (not (children)) ; only look at actions, not projects
                           (todo "NEXT" "TODO")
                           (not (done)))
                     ((org-ql-block-header "\n Overdue")))
       (org-ql-block '(and (todo "WAIT"))
                     ((org-ql-block-header "\n Waiting")))
       (org-ql-block '(closed :on today)
                     ((org-ql-block-header "\n Completed today")))))
     ("." "What's happening"
      ((agenda "" ((org-agenda-span 'day)
                   (org-agenda-start-day "+0d")
                   (org-super-agenda-groups
                    '((:name "Today"
                       :time-grid t
                       :and (:scheduled today
                             :not (:tag ("%quick" "%easy"))
                             :not (:todo ("DONE" "CNCL" "WAIT")))
                       :order 0)
                      (:name "Remove anything else"
                       :discard (:anything t))))))
       (org-ql-block '(and (tags "%quick")
                           (ts-a :on today)
                           (not (todo "WAIT"))
                           (not (done))
                           (not (regexp ,org-ql-regexp-scheduled-with-time)))
                     ((org-ql-block-header "\n Quick")))
       (org-ql-block '(and (tags "%easy")
                           (ts-a :on today)
                           (not (todo "WAIT"))
                           (not (done))
                           (not (regexp ,org-ql-regexp-scheduled-with-time)))
                     ((org-ql-block-header "\n Easy")))
       (org-ql-block '(and (ts-a :to -1)
                           (not (todo "WAIT"))
                           (not (done))
                           (level 2))
                     ((org-ql-block-header "\n Overdue")))
       (org-ql-block '(and (not (scheduled))
                           (not (done))
                           (not (tags "@@someday_maybe"))
                           (level 2))
                     ((org-ql-block-header "\n Unscheduled")))
       (org-ql-block '(and (todo "WAIT"))
                     ((org-ql-block-header "\n Waiting")))
       (org-ql-block '(closed :on today)
                     ((org-ql-block-header "\n Completed today")))
       (org-ql-block '(and (tags ("%quick" "%easy"))
                           (ts-a :from +1 :to +3))
                     ((org-ql-block-header "\n Could pull in"))))))))

(after! evil-org-agenda
  (setq org-super-agenda-header-map (copy-keymap evil-org-agenda-mode-map)))

(use-package! org-ql
  :after org-agenda
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
  
  (defmacro +patch--from-task-location (task &rest body)
    "Runs BODY from the buffer of the task specified by 'task'. This will try to
  find the buffer via the ':org-marker' property in the org element api, or by
  walking up ':parent' tasks until the top, and getting the ':path' property and
  getting a buffer (opening if necessary) for that file"
     `(let ((buffer (if-let ((marker (org-element-property :org-marker ,task)))
                       (marker-buffer marker)
                     (find-file-noselect (+patch--get-path task)))))
       (with-current-buffer buffer
         ,@body)))
  
  (defun +patch--get-contents (task)
    "Using the org element API, get the contents of a task (i.e. the plain text
  after the headline).
  
  If there's a marker, use that (because it's more robust), otherwise recursively
  search up the task tree for a ':path' property (and hope for the best)."
    (+patch--from-task-location task
      (let* ((beg (org-element-property :contents-begin task))
             (end (org-element-property :contents-end task)))
        (when beg (when end (buffer-substring-no-properties beg end))))))
  
  (defconst +patch/org-ql-opened-regexp
    (rx bol ":OPENED:   " (group (1+ not-newline))))
  
  (defun +patch/get-opened-date (task)
    "Get the date a task was opened (i.e. moved from READY to TODO/NEXT) using
  the org element api. Requires fetching the content of the task (which I don't
  have a reliable process for yet)."
    (let* ((opened-prop (org-element-property :OPENED task)))
      (when opened-prop
        (let* ((opened-ts (ts-parse opened-prop))
               (opened-date (ts-format "%Y-%m-%d" opened-ts)))
          opened-date))))
  
  (defun +patch/set-opened-date (&optional pom date)
    "Set the OPENED date of a task."
    (interactive)
    (let* ((pom (or pom (point)))
           (date (or date (org-read-date)))
           (date-str (ts-format "%Y-%m-%d" (ts-parse date))))
      (org-entry-put pom "OPENED" date-str)))
  
  (defun +patch/agenda-set-opened-date (arg &optional date)
    "Set the OPENED date of a task from the org agenda."
    (interactive "P")
    (+patch/set-opened-date (org-get-at-bol 'org-marker) date))
  
  (defun +patch/reopen-task (&optional pom date)
    "Change the OPENED date of a task, and unchedule any scheduled time."
    (org-agenda-schedule '(4))  ;; prefix arg to unschedule
    (+patch/set-opened-date pom date))
  
  (defun +patch/agenda-reopen-task (arg &optional date)
    "Change the OPENED date of a task, and unchedule any scheduled time, from the
  org agenda."
    (interactive "P")
    (+patch/reopen-task (org-get-at-bol 'org-marker) date))
  
  (defun +patch/mark-task-for-planning (&optional pom)
    "Mark a task (at 'pom' or 'point') to be planned in yearly planning (i.e. set
  the 'TO-PLAN' property)."
    (org-entry-put (or pom (point)) "TO-PLAN" ""))
  
  (defun +patch/mark-task-as-planned (&optional pom)
    "Mark a task (at 'pom' or 'point') as planned in yearly planning (i.e. unset
  the 'TO-PLAN' property)."
    (interactive)
    (org-entry-delete (or pom (point)) "TO-PLAN"))
  
  (defun +patch/open-task-after-state-change ()
    "Open a task (by setting the 'OPENED' and 'TO-PLAN' properties). Meant to be
  used in the 'org-after-todo-state-change-hook'."
    (when (equal org-last-state "READY")
      (+patch/set-opened-date (point)
                              (format-time-string
                               "%Y-%m-%d"
                               org-log-note-effective-time))
      (+patch/mark-task-for-planning)))
  (add-hook 'org-after-todo-state-change-hook '+patch/open-task-after-state-change)
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
  
  (org-ql-defpred to-plan (&rest names)
    "Check whether a task needs to be planned (i.e. has a 'TO-PLAN' property)."
    :body (property "TO-PLAN"))
  (setq
   +patch/daily-agenda-super-groups
   `((:name "Today"
      :time-grid t
      :and (:scheduled today
            :not (:tag ("%quick" "%easy"))
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
     ;; TODO omiting this for now, until I decide on semantics for unscheduled project items and action list items
     ;; (:name "Unscheduled"
     ;;  :face error
     ;;  :and (:scheduled nil
     ;;        :not (:todo "DONE")))
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
   `(("Planning" :buffers-files
      ("~/.local/share/notes/gtd/org-gtd-tasks.org")
      :query
      (and
       ;; only include tasks
       ,+patch/is-action
       ;; Get upcoming and unscheduled tasks
       (or (ts :from today :to +45)
           (and (not (scheduled)) (level 2)))
       ;; only get tasks that are still "todo"
       ;; (not (tags "Incubate"))
       (not (todo "WAIT" "DONE" "CNCL"))
       (not (tags "@@someday_maybe")))
      :sort
      (priority todo)
      :narrow nil
      :super-groups ((:name "Unscheduled"
                      :scheduled nil
                      :face error
                      :order 0)
                     (:auto-planning t))
      :title "Planning")
     ("Last Month" :buffers-files
      ("~/.local/share/notes/gtd/org-gtd-tasks.org")
      :query
      (and
       ;; Get upcoming and unscheduled tasks
       (or (ts :from (ts-format "%Y-%m-%d" (make-ts :day 1 :month (ts-month (ts-now)) :year (ts-year (ts-now))))
               :to +45)
           (and (not (scheduled)) (level 2)))
       ;; only get tasks that are still "todo"
       ;; (not (tags "Incubate"))
       (not (todo "WAIT" "DONE" "CNCL"))
       (not (tags "@@someday_maybe")))
      :sort
      (priority todo)
      :narrow nil
      :super-groups ((:name "Unscheduled"
                      :scheduled nil
                      :face error
                      :order 0)
                     (:auto-planning t))
      :title "Last Month")
     ("Daily"
      :buffers-files ("~/.local/share/notes/gtd/org-gtd-tasks.org")
      :query ,+patch/daily-agenda-query
      :sort (priority todo date)
      :narrow nil
      :super-groups ,+patch/daily-agenda-super-groups
      :title "Daily")
     ("Home"
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
       (setq
        +patch/is-top-level-selected-task '(and (todo "TODO" "NEXT")
                                                (not (ancestors (todo "TODO" "NEXT"))))
        +patch/is-planned `(and ,+patch/is-top-level-selected-task
                                (not to-plan))
        +patch/to-be-planned `(and ,+patch/is-top-level-selected-task
                                   (to-plan)))
       
       (+patch/set-orgql-view
        "This Year's Projects"
        `(:buffers-files ("~/.local/share/notes/gtd/org-gtd-tasks.org")
          :query ,+patch/to-be-planned
          :sort (priority todo)
          :narrow nil
          :super-groups ((:auto-outline-path t))
          :title "This Year's Projects"))
       
       (+patch/set-orgql-view
        "Yearly Planning"
        `(:buffers-files ("~/.local/share/notes/gtd/org-gtd-tasks.org")
          :query ,+patch/is-planned
          :sort (priority todo)
          :narrow nil
          :super-groups ((:auto-planning t))
          :title "Yearly Planning"))
       (defun +patch/start-of-this-quarter-ts (&optional as-of)
         (let* ((base-ts (or as-of (ts-now)))
                (base-date (ts-apply :hour 0 :minute 0 :second 0 base-ts))
                (this-month (ts-month base-date))
                (last-month-of-quarter (cond ((< this-month 4) 3)
                                             ((< this-month 7) 6)
                                             ((< this-month 10) 9)
                                             (t 12))))
           (ts-dec 'month 2 (ts-apply :month last-month-of-quarter :day 1 base-date))))
       
       (defun +patch/end-of-this-quarter-ts (&optional as-of)
         (let* ((base-ts (or as-of (ts-now)))
                (base-date (ts-apply :hour 0 :minute 0 :second 0 base-ts))
                (this-month (ts-month base-date))
                (last-month-of-quarter (cond ((< this-month 4) 3)
                                             ((< this-month 7) 6)
                                             ((< this-month 10) 9)
                                             (t 12)))
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
       
       (setq
        planned-for-this-quarter (let* ((end-of-quarter (+patch/end-of-this-quarter-ts)))
                                   `(or (scheduled :to ,(ts-format end-of-quarter))
                                        (ancestors (scheduled :to ,(ts-format end-of-quarter)))))
        scheduled-for-this-quarter `(scheduled :from ,(ts-format (+patch/start-of-this-quarter-ts))
                                     :to ,(ts-format (+patch/end-of-this-quarter-ts)))
        opened-this-quarter `(opened :from ,(ts-format (+patch/start-of-this-quarter-ts))
                              :to ,(ts-format (+patch/end-of-this-quarter-ts))))
       
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
       
       ;; (setq
       
       (+patch/set-orgql-view
        "This Quarter's Projects"
        `(:buffers-files ("~/.local/share/notes/gtd/org-gtd-tasks.org")
          :query (and
                  (todo "TODO" "NEXT")
                  ,+patch/is-action
                  ,scheduled-for-this-quarter
                  (not (scheduled)))
          :sort (priority todo)
          :narrow nil
          :super-groups ((:auto-outline-path t))
          :title "This Quarter's Projects"))
       
       (+patch/set-orgql-view
        "Quarterly Planning"
        `(:buffers-files ("~/.local/share/notes/gtd/org-gtd-tasks.org")
          :query (and
                  (todo "TODO" "NEXT")
                  ,+patch/is-action
                  ,scheduled-for-this-quarter)
          :sort (priority todo)
          :narrow nil
          :super-groups ((:auto-planning t))
          :title ,(format "[Completed last quarter: %s] [Planned for this quarter: %s]" (+patch/num-tasks-completed-last-quarter) (+patch/num-tasks-planned-for-this-quarter))))
       (setq
        scheduled-for-this-week (let* ((today (ts-apply :hour 0 :minute 0 :second 0 (ts-now)))
                                       (dow (ts-day-of-week-num today))
                                       (start-of-week (ts-dec 'day dow today))
                                       (start-of-next-week (ts-inc 'day (- 6 dow) (ts-now)))
                                       (end-of-week (ts-dec 'second 1 start-of-next-week)))
                                  `(scheduled
                                    :from ,(ts-format start-of-week)
                                    :to ,(ts-format end-of-week)))
        scheduled-til-this-week (let* ((today (ts-apply :hour 0 :minute 0 :second 0 (ts-now)))
                                       (dow (ts-day-of-week-num today))
                                       (start-of-week (ts-dec 'day dow today)))
                                  `(scheduled
                                    :to ,(ts-format start-of-week)))
        scheduled-through-this-week (let* ((today (ts-apply :hour 0 :minute 0 :second 0 (ts-now)))
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
        due-this-week-sa (let* ((today (ts-apply :hour 0 :minute 0 :second 0 (ts-now)))
                                (dow (ts-day-of-week-num today))
                                (start-of-week (ts-dec 'day dow today))
                                (start-of-next-week (ts-inc 'day (- 7 dow) (ts-now)))
                                (end-of-week (ts-dec 'second 1 start-of-next-week)))
                           `(before ,(prin1-to-string (ts-format "%Y-%m-%d" start-of-next-week)))))
       
       (+patch/set-orgql-view
        "This Week's Projects"
        `(:buffers-files ("~/.local/share/notes/gtd/org-gtd-tasks.org")
          :query (and
                  (todo "TODO" "NEXT")
                  ,+patch/is-action
                  (not ,scheduled-for-this-week)
                  (or
                   ,opened-through-this-week
                   ,due-this-week))
          :sort (priority todo)
          :narrow nil
          :super-groups ((:name "Upcoming Deadline"
                          :and (:deadline ,due-this-week-sa
                                :not (:todo ("DONE" "CNCL" "WAIT")))
                          :face error
                          :order 0)
                         (:auto-outline-path t))
          :title "This Week's Projects"))
       
       (+patch/set-orgql-view
        "Weekly Planning"
        `(:buffers-files ("~/.local/share/notes/gtd/org-gtd-tasks.org")
          :query (and
                  (todo "TODO" "NEXT")
                  ,+patch/is-action
                  ,scheduled-for-this-week)
          :sort (priority todo)
          :narrow nil
          :super-groups ((:name "Overdue"
                          :and (:scheduled past
                                :face error
                                :not (:todo ("DONE" "CNCL" "WAIT"))))
                         (:auto-planning t))
          :title "Weekly Planning"))


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

(use-package! origami
  :after (org-agenda)
  :hook ((org-agenda-mode . origami-mode)
         (org-agenda-finalize . +patch/org-super-agenda-origami-fold-default))
  :config
  (setq +patch/agenda-auto-hide-groups '("Waiting" "Completed Today" "Could Pull In"))
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

(use-package! parinfer-rust-mode
  :after parinfer
  :custom
  (parinfer-rust-check-before-enable nil))

(use-package! poly-org
  :after org)

(after! org-superstar
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-superstar-prettify-item-bullets t))

(after! org-fancy-priorities
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
                        ("@home"           . ?h)
                        ("@work"           . ?w)
                        ("@comp"           . ?c)
                        ("@cheryls"        . ?y)
                        ("@parents"        . ?p)
                        ("@errands"        . ?r)
                        ("@phone"          . ?o)
                        ("@email"          . ?m)
                        ("@book"           . ?b)
                        ("@anywhere"       . ?a)
                        (:endgroup . nil)
                        ("@@someday_maybe" . ?s)
                        ("@@aspirational"  . ?z)
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
  (setq org-log-into-drawer t))

(use-package burly
  :after org-ql
  :commands (burly-open-bookmark +patch/toggle-quick-agenda-filter +patch/refresh-weekly-planning-view +patch/gen-and-show-daily-agenda)
  :defines (+patch/toggle-quick-agenda-filter +patch/refresh-weekly-planning-view +patch/gen-and-show-daily-agenda)
  :init
  (map! (:map (evil-normal-state-map evil-org-agenda-mode-map org-super-agenda-header-map org-agenda-keymap)
              (:prefix-map ("DEL" . "GTD")
                           (:prefix ("V" . "Planning Views")
                            :desc "Yearly Planning"     "y" (cmd! (burly-open-bookmark "Burly: Yearly Planning"))
                            :desc "Quarterly Planning"  "q" (cmd! (burly-open-bookmark "Burly: Quarterly Planning"))
                            :desc "Weekly Planning"     "w" (cmd! (burly-open-bookmark "Burly: Weekly Planning"))
                            :desc "Refresh Weekly Data" "W" #'+patch/refresh-weekly-planning-view
                            :desc "Daily Planning"      "d" #'+patch/gen-and-show-daily-agenda))
              "<backspace>" nil
              :m "<backspace>" nil
              "<delete>" nil
              :m "<delete>" nil)

        (:leader
         (:prefix "b" :desc "Open Burly Bookmark" "o" #'burly-open-bookmark)))
  :config
  (defun +patch/bookmark-org-ql-view (org-ql-view-name)
      (bookmark-store
       (format "Org QL View: %s" org-ql-view-name)
       (list (cons 'org-ql-view-plist (alist-get org-ql-view-name org-ql-views nil nil #'string=))
             '(handler . org-ql-view-bookmark-handler))
       nil))
  
  (defun gen-burly-split-screen-from-orgql-views (view-name-left view-name-right)
      (+patch/bookmark-org-ql-view view-name-left)
      (+patch/bookmark-org-ql-view view-name-right)
      (let* ((project-frame `(+patch/gen-burly-orgql-view-frame view-name-left 122 66))
             (planning-frame `(leaf (last . t)
                               (parameters
                                (burly-url . ,(burly--bookmark-record-url (bookmark-get-bookmark (format "Org QL View: %s" view-name-right)))))
                               ;; TODO would be nice to remove these props, since I don't understand what they do exactly (and would make this more similar to the previous frame's definition)
                               (buffer (format "*Org QL View: %s*" view-name-right) (hscroll . 0) (fringes 8 8 nil nil) (scroll-bars nil 0 t nil 0 t nil) (vscroll . 0) (point . 1))))
             (window `(nil hc (total-width . 253) (total-height . 66) ,project-frame ,planning-frame))
             (window-filename (concat "?" (url-hexify-string (prin1-to-string window))))
             (window-url (url-recreate-url (url-parse-make-urlobj "emacs+burly+windows" nil nil nil nil window-filename))))
        `((url . ,window-url) (handler . burly-bookmark-handler))))
  
  (defun gen-burly-split-screen-from-orgql-views (view-name-left view-name-right)
      (+patch/bookmark-org-ql-view view-name-left)
      (+patch/bookmark-org-ql-view view-name-right)
      (let* ((project-frame `(leaf (total-width . 126)
                              (total-height . 66)
                              (parameters
                               (burly-url . ,(burly--bookmark-record-url (bookmark-get-bookmark (format "Org QL View: %s" view-name-left)))))
                              (buffer (format "*Org QL View: %s*" view-name-left))))
             (planning-frame `(leaf (last . t)
                               (parameters
                                (burly-url . ,(burly--bookmark-record-url (bookmark-get-bookmark (format "Org QL View: %s" view-name-right)))))
                               ;; TODO would be nice to remove these props, since I don't understand what they do exactly (and would make this more similar to the previous frame's definition)
                               (buffer (format "*Org QL View: %s*" view-name-right) (hscroll . 0) (fringes 8 8 nil nil) (scroll-bars nil 0 t nil 0 t nil) (vscroll . 0) (point . 1))))
             (window `(nil hc (total-width . 253) (total-height . 66) ,project-frame ,planning-frame))
             (window-filename (concat "?" (url-hexify-string (prin1-to-string window))))
             (window-url (url-recreate-url (url-parse-make-urlobj "emacs+burly+windows" nil nil nil nil window-filename))))
        `((url . ,window-url) (handler . burly-bookmark-handler))))
  
  (bookmark-store "Burly: Yearly Planning" (gen-burly-split-screen-from-orgql-views "This Year's Projects" "Yearly Planning") nil)
  (bookmark-store "Burly: Quarterly Planning" (gen-burly-split-screen-from-orgql-views "This Quarter's Projects" "Quarterly Planning") nil)
  (defun +patch/gen-burly-buffer-defaults (buffer-name)
      `(buffer ,buffer-name (selected) (hscroll . 0) (fringes 8 8 nil nil) (margins nil) (scroll-bars nil 0 t nil 0 t nil) (vscroll . 0) (dedicated) (point . 1) (start . 1)))
  
    (defun +patch/gen-burly-orgql-view-frame (view-name width height &optional last-p)
      `(leaf (when last-p (last . t)) (total-width . ,width) (total-height . ,height)
        (parameters
         (burly-url . ,(burly--bookmark-record-url (bookmark-get-bookmark (format "Org QL View: %s" view-name)))))
        ,(+patch/gen-burly-buffer-defaults (format "*Org QL View: %s*" view-name))))
  
    ;; (let* ((view-name-left "This Week's Projects")
  
    (defun +patch/refresh-weekly-planning-view ()
      "We have to refresh the bookmark whenever the burnup chart is re-generated."
      (interactive)
      (+patch/generate-quarters-burnup-plot)
      (+patch/bookmark-org-ql-view "This Week's Projects")
      (+patch/bookmark-org-ql-view "Weekly Planning")
      (bookmark-store "Burly: Weekly Planning"
                    (let* ((view-name-left "This Week's Projects")
                           (view-name-top-right "Weekly Planning")
                           (project-frame (+patch/gen-burly-orgql-view-frame view-name-left 131 66))
                           (planning-frame (+patch/gen-burly-orgql-view-frame view-name-top-right 122 35 t))
                           (burnup-frame `(leaf (last . t) (total-width . 122) (total-height . 31)
                                           (parameters
                                            (burly-url . ,(concat "emacs+burly+bookmark:" "//quarters-burnup.png?"
                                                                  (concat "filename=" (url-hexify-string "\"~/.config/.../.config/doom/modules/lang/org-patch/quarters-burnup.png\"")))))
                                           ,(+patch/gen-burly-buffer-defaults "quarters-burnup.png")))
                           (window `(nil hc (total-width . 253) (total-height . 66) (combination-limit)
                                     ,project-frame
                                     (vc (last . t) (total-width . 122) (total-height . 66) (combination-limit)
                                         ,planning-frame
                                         ,burnup-frame)))
                           (window-filename (concat "?" (url-hexify-string (prin1-to-string window))))
                           (window-url (url-recreate-url (url-parse-make-urlobj "emacs+burly+windows" nil nil nil nil window-filename))))
                      `((url . ,window-url) (handler . burly-bookmark-handler))) nil))
  
    (+patch/refresh-weekly-planning-view)
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
  (defun +patch/refresh-daily-agenda ()
    (bookmark-store "Burly: Daily Planning"
                    (let* ((window '(nil
                                     leaf (total-width . 253)
                                          (total-height . 66)
                                          (parameters (burly-url . "emacs+burly+name://?*Org Agenda*"))
                                          (buffer "*Org Agenda*"
                                                  (selected . t)
                                                  (hscroll . 0)
                                                  (fringes 8 8 nil nil)
                                                  (margins nil)
                                                  (scroll-bars nil 0 t nil 0 t nil)
                                                  (vscroll . 0)
                                                  (dedicated)
                                                  (point . 1649)
                                                  (start . 1))))
                           (window-filename (concat "?" (url-hexify-string (prin1-to-string window))))
                           (window-url (url-recreate-url (url-parse-make-urlobj "emacs+burly+windows" nil nil nil nil window-filename))))
                      `((url . ,window-url) (handler . burly-bookmark-handler))) nil))
  
  (defun +patch/gen-and-show-daily-agenda ()
    "Burly can't load the agenda if it's not already open, so we have to do it ourselves (then refresh the bookmark)."
    (interactive)
    (org-agenda nil ",")
    (+patch/refresh-daily-agenda)
    (burly-open-bookmark "Burly: Daily Planning"))
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
          ,(+patch/get-opened-date raw-task))))

    (defun this-quarters-tasks (&optional as-of)
      (org-ql-query
        :select #'+patch/find-and-parse-task
        :from (cons "~/.local/share/notes/gtd/org-gtd-tasks.org" (f-glob "gtd_archive_[0-9][0-9][0-9][0-9]" "~/.local/share/notes/gtd"))
        :where `(or ,opened-this-quarter
                    ;; keeping scheduled so this quarter is still accurate, but this should be removed afterward
                    ,scheduled-for-this-quarter)))))

(defun +patch/generate-quarters-burnup-plot ()
  (interactive)
  (+patch/invoke-babel-named "~/.config/doom/modules/lang/org-patch/config.org" "quarters-tasks")
  (+patch/invoke-babel-named "~/.config/doom/modules/lang/org-patch/config.org" "plot-quarters-tasks"))

(use-package! transient
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
    (transient-define-infix +patch-gtd/planning/someday-maybe-preset ()
      :class transient-preset
      :arguments '("--todo=READY"))
    
    (transient-define-infix +patch-gtd/planning/in-queue-preset ()
      :class transient-preset
      :arguments `("--todo=TODO" ,(format "--opened-date=%s" (org-read-date nil nil "today"))))
    
    (transient-define-infix +patch-gtd/planning/to-schedule-preset ()
      :class transient-preset
      :arguments `("--todo=NEXT"
                   ,(format "--opened-date=%s"
                            (ts-format "%Y-%m-%d" (ts-apply :hour 0 :minute 0 :second 0 (ts-now))))))
    
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
       ("s" "Someday/Maybe" +patch-gtd/planning/someday-maybe-preset)
       ("q" "In-Queue" +patch-gtd/planning/in-queue-preset)
       ("t" "To-Schedule" +patch-gtd/planning/to-schedule-preset)]
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
    (auto-mode)))
