;;; lang/org-patch/cli.el -*- lexical-binding: t; -*-

(load! "autoload")

;; Tangle the user's config.org before 'doom sync' runs
(when (featurep! :config literate)
  (add-hook 'doom-sync-pre-hook (apply-partially '+literate-patch/literate-tangle-module-h "lang" "org-patch")))
