(use-package! org-gtd
  :defer
  :config
  (setq org-gtd-directory "~/.local/share/notes/gtd/")
  (setq org-gtd-process-item-hooks '(org-set-tags-command))
  (setq org-edna-use-inheritance t)
  (org-edna-mode 1)
  (map! (:leader (:prefix-map ("G" . "GTD")
                  :desc "Capture"             "c" #'org-gtd-capture
                  :desc "Engage"              "e" #'org-gtd-engage
                  :desc "Process Inbox"       "p" #'org-gtd-process-inbox
                  :desc "Show all next"       "n" #'org-gtd-show-all-next
                  :desc "Show stuck projects" "s" #'org-gtd-show-stuck-projects))
        ;; (:map org-gtd-command-map       "C-c C-c" #'org-gtd-clarify-finalize)
        (:map org-gtd-process-map       "C-c C-c" #'org-gtd-choose)))

(setq org-roam-directory "~/.local/share/notes/org-roam/")

(setq org-roam-completion-everywhere t)

(use-package! org-ref
  :defer
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

(use-package! org-capture
  :defer
  :config
  (setq org-capture-templates
        (append org-capture-templates
                (doct '(("Inbox"
                         :keys "i"
                         :file "~/.local/share/notes/gtd/inbox.org"
                         :datetree t
                         :template "* %?"
                         :kill-buffer t)
                        ("Meeting"
                         :keys "m"
                         :children
                         (("Retro"
                           :keys "r"
                           :file "~/.local/share/notes/meetings/retro.org"
                           :datetree t
                           :template ("* %?"
                                      "  %i"
                                      "  %a")
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
  (setq org-roam-completion-system 'default
        org-roam-capture-templates
        (doct-org-roam
         `(:group "Org Roam"
           :file "%<%Y%m%d%H%M%S>-${slug}.org.gpg"
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
  
  (setq org-roam-dailies-directory "dailies/"
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
  )

(define-key global-map "\C-cc" 'org-capture)

(use-package! doct
  :commands doct
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
  )

(defun my/doct-properties ()
                   "Add declaration's :properties to current entry."
                   (let ((properties (doct-get :properties)))
                     (dolist (keyword (seq-filter #'keywordp properties))
                       (org-set-property (substring (symbol-name keyword) 1)
                                         (plist-get properties keyword)))))
;; Usage:
;; (doct '(("My capture template"
;;          ...
;;          :hook my/org-property-drawer
;;          :properties (:anki_deck "${category}"))))

(setq org-agenda-start-with-clockreport-mode t)

(setq org-agenda-clockreport-parameter-plist
      '(:link t :maxlevel 2 :formula "$5=$3+$4;t::$6=ceil($5*60/25);N"))

(defmacro η (fnc)
  "Return function that ignores its arguments and invokes FNC."
  `(lambda (&rest _rest)
     (funcall ,fnc)))

(advice-add 'org-deadline       :after (η #'org-save-all-org-buffers))
(advice-add 'org-schedule       :after (η #'org-save-all-org-buffers))
(advice-add 'org-store-log-note :after (η #'org-save-all-org-buffers))
(advice-add 'org-todo           :after (η #'org-save-all-org-buffers))

(after! org-agenda
  (org-super-agenda-mode))

; TODO review these config options
(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      ;; org-agenda-block-separator nil
      org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-compact-blocks t)

(setq org-agenda-custom-commands
      '(("." "What's happening"
         ((agenda "" ((org-agenda-span 'day)
                      (org-agenda-start-day "+0d")
                      (org-super-agenda-groups
                       '((:name "Today"
                          :time-grid t
                          :discard (:todo "NEXT"
                                    :todo "WAIT")
                          :and (:scheduled today
                                :not (:todo "NEXT")
                                :not (:todo "WAIT"))
                          :order 0)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-agenda-span 'week)
                       (org-agenda-start-day "+0d")
                       (org-super-agenda-groups
                        '((:name "Overdue"
                           :scheduled past
                           :face error
                           :order 2)
                          (:name "Next to do"
                           :and (:scheduled today
                                 :todo "NEXT")
                           :discard (:and (:scheduled today
                                           :and (:not (:todo "NEXT") :not (:todo "WAIT"))))
                           :order 1)
                          (:name "Waiting"
                           :and (:scheduled today
                                 :todo "WAIT")
                           :order 1)
                          (:name "Unscheduled"
                           :scheduled nil
                           :face error
                           :order 2)))))))))

(after! evil-org-agenda
  (setq org-super-agenda-header-map evil-org-agenda-mode-map))

(setq deft-directory "~/.local/share/notes")
(setq deft-recursive t)

(setq org-directory "~/.local/share/notes")

(setq org-startup-with-latex-preview t)

;(setq org-startup-truncated nil)
;(setq org-startup-indented t)

;(setq org-refile-targets
;      '((nil :maxlevel . 3)
;        (org-agenda-files :maxlevel . 3)))

(setq org-confirm-babel-evaluate nil)

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(use-package! ob-mermaid
  :defer
  :config
  (setq ob-mermaid-cli-path "/usr/local/bin/mmdc"))

(setq org-pomodoro-length 40)
(setq org-pomodoro-short-break-length 10)
(setq org-pomodoro-long-break-length 20)
(setq org-pomodoro-play-sounds 0)
;(setq alert-default-style 'growl)

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
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-superstar-prettify-item-bullets t ))

(setq org-ellipsis " ▾ "
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
  :defer
  :after org
  :config
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))
