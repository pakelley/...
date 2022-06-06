(use-package! bazel
  :defines local-bazel-test +bazel-patch/activate-venv +bazel-patch/build-venv
  :commands local-bazel-test +bazel-patch/activate-venv +bazel-patch/build-venv
  :init
  (map! :localleader
        :map python-mode-map
        (:prefix-map ("b" . "bazel")
         :desc "test" "    t" #'+bazel-patch/run-tests
         (:prefix-map ("e" . "env")
          :desc "build"    "b" #'+bazel-patch/build-venv
          :desc "activate" "a" #'+bazel-patch/activate-venv)))
  :config
  (defcustom +bazel-patch/local-target nil
    "dir-locals defined target for bazel commands"
    :type 'string)
  (setq +bazel-patch/source-root "/Users/pkelley/workspace/source")
  (defun bazel-build-venv (target venv-location)
    (bazel--compile "build-venv" target "--location" venv-location))
  
  (defun +bazel-patch/build-venv-helper (source-root local-target)
    (let* ((project-root (concat
                          (file-name-as-directory source-root)
                          (replace-regexp-in-string "//" "" (replace-regexp-in-string ":.*" "" local-target))))
           (default-directory source-root)
           (venv-location (concat (file-name-as-directory project-root) ".venv"))
           (builder-command (concat (file-name-as-directory source-root)
                                    "sandbox/users/gcasassaez/bazel2vscode/bazel2vscode.py")))
  
      (when (file-directory-p venv-location)
            (delete-directory venv-location :recursive t))
      (bazel-build-venv local-target venv-location)
      ; KLUDGE Run bazel cmds synchronously, despite the bazel lib using async `compile` function
      ;        This is done by chaining subsequent commands to `compilation-finish-functions`
      ;        and only running the next command if the previous one was successful (by
      ;        checking that the message from the last command was the success message:
      ;        "finished\n")
      (defun build-bazel-workspace-file (builder message)
        (when (string-equal message "finished\n")
          (compile (mapconcat #'shell-quote-argument
                          (list builder-command
                                "--open"
                                local-target)
                          " ")))
        (remove-hook 'compilation-finish-functions #'build-bazel-workspace-file))
  
      (defun build-bazel-local-target (builder message)
        (when (string-equal message "finished\n")
          (bazel-build (concat local-target "_file_infos.json"))
          (add-hook 'compilation-finish-functions #'build-bazel-workspace-file))
        (remove-hook 'compilation-finish-functions #'build-bazel-local-target))
  
      (add-hook 'compilation-finish-functions #'build-bazel-local-target)))
  
  (defun +bazel-patch/build-venv ()
    ; NOTE It's important to call out to a separate function so the args are bound
    ;      (otherwise the config vals changing will affect the long-running process)
    (interactive)
    (+bazel-patch/build-venv-helper +bazel-patch/source-root +bazel-patch/local-target))
  (defun +bazel-patch/activate-venv ()
    (interactive)
    (let* ((project-root (concat
                          (file-name-as-directory +bazel-patch/source-root)
                          (replace-regexp-in-string "//" "" (replace-regexp-in-string ":.*" "" +bazel-patch/local-target))))
           (source-roots-workspace-file
            (concat (file-name-as-directory +bazel-patch/source-root)
                    ".vscode/"
                    (file-name-as-directory (replace-regexp-in-string ":" "__" (replace-regexp-in-string "/" "_" +bazel-patch/local-target)))
                    (replace-regexp-in-string ".*:" "" +bazel-patch/local-target)
                    ".code-workspace"))
           (extra-paths (cdr (assoc 'python.analysis.extraPaths
                                    (assoc 'settings
                                           (json-read-file source-roots-workspace-file)))))
           (venv-location (concat (file-name-as-directory project-root) ".venv")))
      (save-window-excursion
        (modify-dir-local-variable 'python-mode 'lsp-pyright-venv-path venv-location 'add-or-replace)
        (modify-dir-local-variable 'python-mode 'lsp-pyright-extra-paths extra-paths 'add-or-replace))
      (message "Loaded bazel env into `.dir-locals.el`. Reload buffer for env to take effect.")))
  (defun +bazel-patch/run-tests ()
      "Run bazel test for local dir"
      (interactive)
      (bazel-test +bazel-patch/local-target))
  )
