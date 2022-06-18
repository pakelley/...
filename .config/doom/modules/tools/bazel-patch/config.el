(use-package! bazel
  :defines local-bazel-test +bazel-patch/activate-venv +bazel-patch/build-venv
  :commands local-bazel-test +bazel-patch/activate-venv +bazel-patch/build-venv
  :init
  (map! :localleader
        :map python-mode-map
        (:prefix-map ("b" . "build")
         :desc "test"      "t" #'+bazel-patch/run-tests
         (:prefix-map ("e" . "env")
          :desc "build"    "b" #'+bazel-patch/build-venv-from-dir-locals
          :desc "setup"    "s" #'+bazel-patch/setup-venv-dir-locals
          :desc "refresh"  "r" #'+bazel-patch/refresh-venv-dir-locals)))
  :config
  (defcustom +bazel-patch/local-target nil
    "dir-locals defined target for bazel commands"
    :type 'string)
  (setq +bazel-patch/source-root "/Users/pkelley/workspace/source")
  (defun bazel-build-venv (target venv-location)
    (bazel--compile "build-venv" target "--location" venv-location))
  
  (defun +bazel-patch/build-venv-internal (source-root local-target)
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
      ; KLUDGE Use lexical-let to bind local-target in bazel functions
      (eval-when-compile (require 'cl-lib))
      (lexical-let
       ((local-target local-target)
        (builder-command builder-command))
  
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
         (remove-hook 'compilation-finish-functions #'build-bazel-local-target)))
  
      (add-hook 'compilation-finish-functions #'build-bazel-local-target)))
  
  (defun +bazel-patch/build-venv (local-target)
    (interactive (list (bazel--read-target-pattern "target" nil)))
    (+bazel-patch/build-venv-internal +bazel-patch/source-root local-target))
  (defun +bazel-patch/gen-venv-location (local-target)
    (let* ((project-root (concat
                          (file-name-as-directory +bazel-patch/source-root)
                          (replace-regexp-in-string "//" "" (replace-regexp-in-string ":.*" "" local-target)))))
      (concat (file-name-as-directory project-root) ".venv")))
  
  (defun +bazel-patch/gen-extra-paths (local-target)
    (let* ((project-root (concat
                          (file-name-as-directory +bazel-patch/source-root)
                          (replace-regexp-in-string "//" "" (replace-regexp-in-string ":.*" "" local-target))))
           (source-roots-workspace-file
            (concat (file-name-as-directory +bazel-patch/source-root)
                    ".vscode/"
                    (file-name-as-directory (replace-regexp-in-string ":" "__" (replace-regexp-in-string "/" "_" local-target)))
                    (replace-regexp-in-string ".*:" "" local-target)
                    ".code-workspace")))
      (cdr (assoc 'python.analysis.extraPaths
                  (assoc 'settings
                         (json-read-file source-roots-workspace-file))))))
  
  (defun +bazel-patch/activate-venv (local-target)
    (interactive (list (bazel--read-target-pattern "target" nil)))
    (let* ((extra-paths (+bazel-patch/gen-extra-paths local-target))
           (venv-location (+bazel-patch/gen-venv-location local-target)))
      (setq-local lsp-pyright-venv-path venv-location
                  lsp-pyright-extra-paths extra-paths)
      (message "Loaded bazel env")))
  
  
  (defun +bazel-patch/build-venv-from-dir-locals ()
    ; NOTE It's important to call out to a separate function so the args are bound
    ;      (otherwise the config vals changing will affect the long-running process)
    (interactive)
    (if (and (boundp '+bazel-patch/local-target) +bazel-patch/local-target)
        (+bazel-patch/build-venv +bazel-patch/local-target)
        (call-interactively '+bazel-patch/build-venv)))
  (defun +bazel-patch/set-local-target (local-target)
    (interactive (list (bazel--read-target-pattern "target" nil)))
    (save-window-excursion
        (modify-dir-local-variable major-mode '+bazel-patch/local-target local-target 'add-or-replace)))
  
  (defun +bazel-patch/refresh-venv-dir-locals (local-target)
    (interactive (list (bazel--read-target-pattern "target" nil)))
    (let* ((extra-paths (+bazel-patch/gen-extra-paths local-target))
           (venv-location (+bazel-patch/gen-venv-location local-target)))
      (save-window-excursion
        (modify-dir-local-variable major-mode 'lsp-pyright-venv-path venv-location 'add-or-replace)
        (modify-dir-local-variable major-mode 'lsp-pyright-extra-paths extra-paths 'add-or-replace))
      (message "Loaded bazel env into `.dir-locals.el`. Reload buffer for env to take effect.")))
  
  (defun +bazel-patch/setup-venv-dir-locals (local-target)
    (interactive (list (bazel--read-target-pattern "target" nil)))
    (+bazel-patch/set-local-target local-target)
    (+bazel-patch/refresh-venv-dir-locals local-target))
  (defun +bazel-patch/run-tests ()
      "Run bazel test for local dir"
      (interactive)
      (if (and (boundp '+bazel-patch/local-target) +bazel-patch/local-target)
        (bazel-test +bazel-patch/local-target)
        (call-interactively 'bazel-test)))
  )

(defun +bazel-patch/open-ipython (local-target)
  (interactive (list (bazel--read-target-pattern "target" nil)))
  (eval-when-compile (require 'python))
  (let ((python-shell-interpreter (concat (+bazel-patch/gen-venv-location local-target)
                                          "/bin/ipython"))
        (python-shell-interpreter-args "-i --matplotlib=inline --automagic --simple-prompt --pprint"))
    (pop-to-buffer
     (process-buffer
      (run-python nil nil t)))))
