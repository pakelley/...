(use-package! polymode
  :defer t
  :config
  (map! :localleader
        :map poly-noweb+r-mode-map
        "e" #'polymode-export)
  )
(use-package! poly-R
  :defer t)

(use-package! ess
  :defer t
  :config
  (defun ess-insert-magrittr-pipe ()
    (interactive)
    (just-one-space 1)
    (insert "%>%")
    (reindent-then-newline-and-indent))
  (map!
   (:map ess-r-mode-map
    "M--" #'ess-insert-assign
    "M-." #'ess-insert-magrittr-pipe)
   (:map inferior-ess-r-mode-map
    "M--" #'ess-insert-assign
    "M-." #'ess-insert-magrittr-pipe)))
