(use-package! org-gtd
  :config
  (map! (:leader (:prefix-map ("G" . "GTD")
                  :desc "Capture"             "c" #'org-gtd-capture
                  :desc "Process Inbox"       "p" #'org-gtd-process-inbox
                  :desc "Show all next"       "n" #'org-gtd-show-all-next
                  :desc "Show stuck projects" "s" #'org-gtd-show-stuck-projects))
        (:map org-gtd-command-map       "C-c C-c" #'org-gtd-clarify-finalize)))

(setq org-gtd-directory "~/.local/share/notes/gtd/")

(setq org-gtd-process-item-hooks '(org-set-tags-command)
      org-agenda-property-list '("DELEGATED_TO")
      org-agenda-custom-commands '(("g" "Scheduled today and all NEXT items" ((agenda "" ((org-agenda-span 1))) (todo "NEXT")))))

(setq org-edna-use-inheritance t)
(org-edna-mode 1)

(setq org-roam-directory "~/.local/share/notes/org-roam/")

(setq org-roam-completion-everywhere t)

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

(setq doct-after-conversion-functions '(doct-org-roam-convert))

(cl-defmethod org-roam-node-category ((node org-roam-node))
  "Return the namespace for NODE.
The namespace is the final directory of the file for the node."
  (interactive
   (list (completing-read-multiple "Tag: " (org-roam-tag-completions))))

(setq org-roam-completion-system 'default
      org-roam-capture-templates
      (doct-org-roam
       '(:group "Org Roam"
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
           ; HACK include filetag header in template so I can re-use `category` (and therefore only prompt for it once)
           :template ("#+filetags: :${category}:"
                      "* ${title}"
                      ":PROPERTIES:"
                      ":ANKI_DECK: ${category}"
                      ":END:"
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

(use-package! org-ref
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

(setq org-capture-templates
      '(("b" "Bombora" entry (file+datetree "~/SparkleShare/org/work/bombora/bombora.cap.org")
         "* %?\n  %i\n  %a")
        ("B" "Bombora Retro" entry (file+datetree "~/SparkleShare/org/work/bombora/bombora.retro.org")
         "* %?\n  %i\n  %a")
        ("i" "Inbox"
         entry (file "~/.local/share/notes/gtd/inbox.org")
         "* %?\n%U\n\n  %i"
         :kill-buffer t)
        ("l" "Todo with link"
         entry (file "~/.local/share/notes/gtd/inbox.org")
         "* %?\n%U\n\n  %i\n  %a"
         :kill-buffer t)))

(setq org-agenda-start-with-clockreport-mode t)

(setq org-agenda-clockreport-parameter-plist
      '(:link t :maxlevel 2 :formula "$5=$3+$4;t::$6=ceil($5*60/25);N"))

(setq org-agenda-files `(,org-gtd-directory))

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
