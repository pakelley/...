(use-package! anaconda-mode
  :defer t
  :config
  (setq conda-anaconda-home "/opt/miniconda3")
  (setq conda-env-home-directory "/Users/pakelley/.config/conda")
  (conda-env-autoactivate-mode t)
  )

(use-package! numpydoc
  :custom
  (numpydoc-insertion-style 'yas)
  :config
  (map! :localleader ("d" #'numpydoc-generate)))
