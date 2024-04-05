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
  (setq blacken-executable "blue")
  (defun +patch/set-apheleia-black-executable (exec-path)
    (setf (car (cdr (assoc 'black apheleia-formatters))) exec-path))

  (advice-add 'blacken-buffer :before (lambda () (setq blacken-executable (format "%s.venv/bin/blue" (project-root (project-current))))))
  (advice-add 'blacken-buffer :after (lambda () (setq blacken-executable "blue")))
  (advice-add 'blacken-buffer :before (lambda () (+patch/set-apheleia-black-executable (format "%s.venv/bin/blue" (project-root (project-current))))))
  (advice-add 'blacken-buffer :after (lambda () (+patch/set-apheleia-black-executable "blue")))
  )

(use-package! flymake-ruff
  :hook ((python-mode eglot-managed-mode) . flymake-ruff-load)
  :config
  (map! :map python-mode-map
        (:localleader
         (:prefix ("l" . "lint")
          :desc "flymake list" "l" #'consult-flymake)))
  ;; use the `ruff` installed for the current project
  (advice-add 'flymake-ruff--check-buffer :before (lambda () (setq flymake-ruff-program (format "%s.venv/bin/ruff" (project-root (project-current))))))
  (advice-add 'flymake-ruff--check-buffer :after (lambda () (setq flymake-ruff-program "ruff"))))

(use-package! reformatter
  :config
  (reformatter-define ruff-format
                      :program "ruff"
                      :args '("--fix" "-")
                      :group 'python))

(after! reformatter
  (reformatter-define docformatter-format
                      :program "docformatter"
                      :args '("--style" "numpy" "--in-place" "-")
                      :group 'python)
  (reformatter-define pyment-format
                      :program "pyment"
                      :args '("--output" "numpydoc" "-")
                      :group 'python))

(defun +patch-python/lint ()
  (py-isort-before-save)
  ;; (pyment-format-buffer)
  (docformatter-format-buffer)
  (ruff-format-buffer)
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

(use-package! python-pytest
  :config
  (advice-add 'python-pytest--run :before (lambda (&rest args) (setq python-pytest-executable (format "%s.venv/bin/pytest" (project-root (project-current))))))
  (advice-add 'python-pytest--run :after (lambda (&rest args) (setq python-pytest-executable "pytest")))
  )

;; (use-package! ts-docstr)
