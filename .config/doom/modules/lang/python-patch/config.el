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

(defun +patch/get-pdm-packages-path ()
  "For the current PDM project, find the path to the packages."
  (let ((packages-path (string-trim (shell-command-to-string "pdm info --packages"))))
    (concat packages-path "/lib")))

(defun +patch/eglot-workspace-config (server)
  "For the current PDM project, dynamically generate a python lsp config."
  `(:python\.analysis (:extraPaths ,(vector (+patch/get-pdm-packages-path)))))

(setq-default eglot-workspace-configuration #'+patch/eglot-workspace-config)

;; while we're at it, remove doom's hook to stop hearing errors about it
(remove-hook! 'python-mode-local-vars-hook #'+python-init-anaconda-mode-maybe-h)
