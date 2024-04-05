(after! (oc citar)
  (setq
   org-cite-global-bibliography '("~/.local/share/bib/babel.bib" "~/.local/share/bib/human-signal.bib"))
  (setq citar-bibliography org-cite-global-bibliography))

(after! oc (setq org-cite-csl-styles-dir "~/Zotero/styles"))

(use-package! citar-org-roam
  :after (citar org-roam)
  :custom
  (citar-org-roam-subdir "literature-notes")
  ;; Tell citar-org-roam which capture template to use (as suggested in the README)
  (citar-org-roam-capture-template-key "n")
  )
