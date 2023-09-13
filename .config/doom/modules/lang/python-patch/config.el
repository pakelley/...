(use-package! numpydoc
  :after python
  :custom
  (numpydoc-insertion-style 'yas)
  :config
  (map! :localleader
        :map python-mode-map
        ("d" #'numpydoc-generate)))

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
  `((:python .
     (:venvPath "."
      :venv ".venv"
      :pythonPath  "./.venv/bin/python"))))

(setq-default eglot-workspace-configuration #'+patch/eglot-pyright-venv-workspace-config)

;; while we're at it, remove doom's hook to stop hearing errors about it
(remove-hook! 'python-mode-local-vars-hook #'+python-init-anaconda-mode-maybe-h)

(use-package! blacken
  :config
  (setq blacken-executable "blue"))

(defun +patch-python/lint ()
  (py-isort-before-save)
  (blacken-buffer))

;;;###autoload
(define-minor-mode +patch-python/lint-mode
  "Automatically lint before saving."
  :lighter " PP/lint"
  (if +patch-python/lint-mode
      (add-hook 'before-save-hook '+patch-python/lint nil t)
    (remove-hook 'before-save-hook '+patch-python/lint t)))
;; (add-hook 'before-save-hook #'+patch-python/lint)
(add-hook 'python-mode-hook #'+patch-python/lint-mode)

(use-package! flymake-ruff
  :hook ((python-mode eglot-managed-mode) . flymake-ruff-load))
