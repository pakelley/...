(use-package! org-gtd
  :after org
  :init
  (setq org-gtd-directory "~/.local/share/notes/gtd/")
  (setq org-gtd-process-item-hooks '(org-set-tags-command))
  (setq org-edna-use-inheritance t)
  (org-edna-mode 1)
  (map! (:leader (:prefix-map ("G" . "GTD")
                  :desc "Capture"             "c" #'org-gtd-capture
                  :desc "Engage"              "e" #'org-gtd-engage
                  :desc "Process Inbox"       "p" #'org-gtd-process-inbox
                  :desc "Show all next"       "n" #'org-gtd-show-all-next
                  :desc "Show stuck projects" "s" #'org-gtd-show-stuck-projects
                  :desc "Capture"             "c" #'org-gtd-capture
                  :desc "Archive Done"        "a" #'org-gtd-archive-completed-items)
         (:map org-gtd-command-map       "C-c C-c" #'org-gtd-choose))))

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
  ; start org-roam on startup
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

(setq org-todo-keywords
      '((sequence "NEXT(n)" "TODO(t!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@!)" "TRASH(r!)")))
(setq org-todo-keyword-faces
      '(("NEXT" . (:foreground "#f0dfaf" :weight bold))
        ("WAIT" . (:foreground "#dc8cc3" :weight bold))
        ("CANCELED" . (:foreground "#8cd0d3" :weight bold))
        ("TRASH" . (:foreground "#dfaf8f" :weight bold))))

(define-key global-map "\C-cc" 'org-capture)

(use-package! doct
  :after (org org-capture)
  :commands doct
  :custom
  ; setq
  (org-capture-templates
        (append org-capture-templates
                (doct '(("Inbox"
                         :keys "i"
                         :file "~/.local/share/notes/gtd/inbox.org"
                         :template "* %?"
                         :kill-buffer t)
                        ("Today"
                         :keys "2"
                         :file "~/.local/share/notes/gtd/org-gtd-tasks.org"
                         :olp ("Calendar")
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
                          ("Nico 1-on-1"
                           :keys "n"
                           :file "~/.local/share/notes/meetings/nico.org"
                           :datetree t
                           :template "* %?"
                           :kill-buffer t)))
                        ("Shopping" :keys "s"
                         :file "~/.local/share/notes/reference/shopping.org"
                         :template "* %?"
                         :children
                         (("Home" :keys "h" :olp ("Home"))
                          ("Christmas" :keys "c" :olp ("Christmas"))
                          ("Gift" :keys "g" :olp ("Gifts")) ; TODO either add recipient as tag or in olp
                          ("Groceries" :keys "o" :olp ("Groceries"))))
                        (:group "Reference"
                         :file "~/.local/share/notes/reference/capture.org"
                         :template "* %?"
                         :children
                         (("Food"
                           :keys "f"
                           :children
                           (("Recipe"     :keys "r" :olp ("Recipes"))
                            ("Cocktail"   :keys "c" :olp ("Cocktails"))
                            ("Restaurant" :keys "s" :olp ("Restaurants"))))
                          ("Media" :keys "e"
                           :children
                           (("Movie"   :keys "m" :olp ("Movies"))
                            ("Show"    :keys "s" :olp ("Shows"))
                            ("Book"    :keys "b" :olp ("Books"))
                            ("Article" :keys "a" :olp ("Articles"))
                            ("Album"   :keys "l" :olp ("Albums"))))
                          ("Repo" :keys "r" :olp ("Repos"))))))))
  )

(use-package! org-agenda
  :commands org-agenda
  :custom
  (org-agenda-files '("~/.local/share/notes/gtd/org-gtd-tasks.org"))
  :config
  (defmacro ?? (fnc)
    "Return function that ignores its arguments and invokes FNC."
    `(lambda (&rest _rest)
       (funcall ,fnc)))
  
  (advice-add 'org-deadline       :after (?? #'org-save-all-org-buffers))
  (advice-add 'org-schedule       :after (?? #'org-save-all-org-buffers))
  (advice-add 'org-store-log-note :after (?? #'org-save-all-org-buffers))
  (advice-add 'org-todo           :after (?? #'org-save-all-org-buffers))
  (advice-add 'org-refile         :after (?? #'org-save-all-org-buffers))
  (run-with-idle-timer 300 t (lambda () (save-window-excursion (org-agenda nil "."))))
  (defun org-agenda-reschedule-to-today (&optional arg)
    "Reschedule selected task(s) for today."
    (interactive "P")
    (org-agenda-schedule arg "."))
  
  (setq org-agenda-bulk-custom-functions '((?. org-agenda-reschedule-to-today)))
  (map! (:map org-agenda-mode-map "." #'org-agenda-reschedule-to-today)
        (:map evil-org-agenda-mode-map :m "." #'org-agenda-reschedule-to-today)))

(use-package! org-refile
  :after org-agenda
  :config
  (add-to-list 'org-refile-targets `(,(directory-files "~/.local/share/notes/reference" t ".*\\.org$") :maxlevel . 3))
  (add-to-list 'org-refile-targets `(,(directory-files "~/.local/share/notes/gtd" t ".*\\.org$") :maxlevel . 3)))

(after! org-agenda
  (org-super-agenda-mode))

(use-package! org-super-agenda
  :after (org-ql org-agenda)
  :commands org-super-agenda-mode
  :config
  ; TODO review these config options
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        ;; org-agenda-block-separator nil
        org-agenda-tags-column 100 ;; from testing this seems to be a good value
        org-agenda-compact-blocks t)

  (setq org-agenda-custom-commands
        `(("." "What's happening"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-start-day "+0d")
                        (org-super-agenda-groups
                         '((:name "Today"
                            :time-grid t
                            :discard (:todo "NEXT"
                                      :todo "WAIT")
                            :and (:not (:todo "NEXT")
                                  :not (:todo "WAIT"))
                            :order 0)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-agenda-span 'week)
                         (org-agenda-start-day "+0d")
                         (org-super-agenda-groups
                          '((:name "Waiting"
                             :todo "WAIT"
                             :order 2)
                            (:name "Overdue"
                             :scheduled past
                             :face error
                             :order 3)
                            (:name "Unscheduled"
                             :scheduled nil
                             :face error
                             :order 3)
                            (:name "Remove NEXT tasks that will already appear in clock agenda"
                             :discard (:regexp ,org-ql-regexp-scheduled-with-time))
                            (:name "Quick"
                             :and (:scheduled today
                                   :todo "NEXT")
                             :discard (:and (:scheduled today
                                             :and (:not (:todo "NEXT") :not (:todo "WAIT"))))
                             :order 1)
                            (:name "Could pull in"
                             :order 4
                             :todo "NEXT")
                            (:name "Remove anything else"
                             :discard (:anything t)))))))))))

(after! evil-org-agenda
  (setq org-super-agenda-header-map evil-org-agenda-mode-map))

(use-package! origami
  :after org-agenda
  :hook
  (org-agenda-mode . origami-mode)
  (org-agenda-finalize . +patch/org-super-agenda-origami-fold-default)
  :config

  (setq +patch/agenda-auto-show-groups
    '("Today" "Quick" "Waiting"))

  (defun +patch/org-super-agenda-origami-fold-default ()
    "Fold certain groups by default in Org Super Agenda buffer."
    (forward-line 2)
    (cl-loop do (origami-forward-toggle-node (current-buffer) (point))
             while (numberp (org-agenda-forward-block)))
    (--each +patch/agenda-auto-show-groups
      (goto-char (point-min))
      (when (re-search-forward (rx-to-string `(seq bol " " ,it)) nil t)
        (origami-show-node (current-buffer) (point)))))

  (defun +patch/dont-show-waiting-in-agenda ()
    (interactive)
    (setq +patch/agenda-auto-show-groups
          (remove "Waiting" +patch/agenda-auto-show-groups))
    (org-agenda-redo))

  (defun +patch/show-waiting-in-agenda ()
    (interactive)
    (setq +patch/agenda-auto-show-groups
          (cons "Waiting" +patch/agenda-auto-show-groups))
    (org-agenda-redo))

  (map!
   (:map evil-org-agenda-mode-map "TAB" #'origami-toggle-node)
   (:map org-super-agenda-header-map :m "<tab>" #'origami-toggle-node)
   (:map org-super-agenda-header-map :m "TAB" #'origami-toggle-node)
   (:map org-super-agenda-header-map "TAB" #'origami-toggle-node)
   :map org-agenda-mode-map
   :localleader
   ("w" #'+patch/show-waiting-in-agenda)
   ("W" #'+patch/dont-show-waiting-in-agenda)))

(setq deft-directory "~/.local/share/notes")
(setq deft-recursive t)

(setq org-directory "~/.local/share/notes")

(setq org-startup-with-latex-preview t)

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
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))

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

(setq org-tag-alist
      '(
        ("Configuration")
        ; GTD
        (:startgrouptag)
        ("GTD")
        (:grouptags)
        ("Control")
        ("Persp")
        (:endgrouptag)
        (:startgrouptag)
        ("Control")
        (:grouptags)
        ("Context")
        ("Task")
        (:endgrouptag)
        ("Circuit Theory")))

(after! org-superstar
  (setq org-superstar-headline-bullets-list '("???" "???" "???" "???" "???" "???" "???" "???")
        org-superstar-prettify-item-bullets t ))

(setq org-ellipsis " ??? "
      org-hide-leading-stars t
      org-priority-highest ?A
      org-priority-lowest ?E
      org-fancy-priorities-list
      `(,(list ?A (all-the-icons-octicon "flame" :face 'all-the-icons-red))
        ,(list ?B (all-the-icons-faicon "bolt" :face 'all-the-icons-orange))
        ,(list ?C (all-the-icons-faicon "check" :face 'all-the-icons-yellow))
        ,(list ?D (all-the-icons-faicon "beer" :face 'all-the-icons-green))
        ,(list ?E (all-the-icons-faicon "bed" :face 'all-the-icons-blue)))
      )

(use-package! org-modern
  :after org
  :custom
  (org-modern-priority nil)
  (org-modern-internal-target `(,(all-the-icons-material "redo" :face 'all-the-icons-blue) t " "))
  :config
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

