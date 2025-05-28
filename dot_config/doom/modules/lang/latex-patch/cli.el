;;; lang/latex-patch/cli.el -*- lexical-binding: t; -*-

(load! "autoload")

;; Tangle the user's config.org before 'doom sync' runs
(when (modulep! :config literate)
  (add-hook 'doom-before-sync-hook (apply-partially '+literate-patch/literate-tangle-module-h "lang" "scala-patch")))
