(after! (oc citar)
  (setq
   org-cite-global-bibliography '("~/.local/share/bib/babel.bib" "~/.local/share/bib/human-signal.bib"))
  (setq citar-bibliography org-cite-global-bibliography))

(after! oc (setq org-cite-csl-styles-dir "~/Zotero/styles"))
