(use-package! smartparens
  :defer t
  :commands (sp-forward-slurp-sexp sp-forward-barf-sexp sp-backward-slurp-sexp sp-backward-barf-sexp)
  :config
  (map!
   (:map sp-keymap
    "<C-right>" #'sp-forward-slurp-sexp
    "<C-left>" #'sp-backward-slurp-sexp
    "<C-M-right>" #'sp-forward-barf-sexp
    "<C-M-left>" #'sp-backward-barf-sexp)
   (:map global-map
    "<C-M-right>" nil
    "<C-M-left>" nil)))
