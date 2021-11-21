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

(setq org-roam-completion-system 'default
      org-roam-capture-templates
       '(("d" "default" plain "%?"
          :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg" "#+title: ${title}\n")
          :unnarrowed t)
         ("ll" "link note" plain
          "* %^{Link}"
          :target (file+olp "Inbox" ("Links"))
          :unnarrowed t
          :immediate-finish)
         ("lt" "link task" entry
          "* TODO %^{Link}"
          :target (file+olp "Inbox" ("Tasks"))
          :unnarrowed t
         :immediate-finish)))
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

(setq doom-theme 'doom-oceanic-next)
(setq doom-localleader-key ",")

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
