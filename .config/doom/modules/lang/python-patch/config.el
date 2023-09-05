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

(defun +patch/get-pdm-venv-packages-path ()
  "For the current PDM project (using venv, rather than pep-582, to install
packages), find the path to the packages."
  (let ((packages-path
         (string-trim (shell-command-to-string "pdm venv --path in-project"))))
    (concat packages-path "/lib/python3.10/site-packages")))

(defun +patch/get-pdm-pep582-packages-path ()
  "For the current PDM project (using pep-582, rather than venv, to install
packages), find the path to the packages."
  (let ((packages-path
         (string-trim (shell-command-to-string "pdm info --packages"))))
    (concat packages-path "/lib")))

;; I don't think this works anymore... but I'm using the pyright-specific
;; method below. Keeping this around in case I migrate away from pyright.
(defun +patch/eglot-extraPaths-workspace-config (server)
  "For the current PDM project, dynamically generate a python lsp config."
  `(:python\.analysis (:extraPaths ,(vector (+patch/get-pdm-venv-packages-path)))))

(defun +patch/eglot-pyright-venv-workspace-config (server)
  "For the current PDM project, dynamically generate a python lsp config."
  `((:pyright .
     (:venvPath ,(projectile-project-root)
      :venv ".venv"
      :pythonPath  "./.venv/bin/python"))))

(setq-default eglot-workspace-configuration #'+patch/eglot-pyright-venv-workspace-config)

;; while we're at it, remove doom's hook to stop hearing errors about it
(remove-hook! 'python-mode-local-vars-hook #'+python-init-anaconda-mode-maybe-h)
