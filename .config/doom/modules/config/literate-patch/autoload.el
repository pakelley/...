;;; config/literate-patch/new-autoload.el -*- lexical-binding: t; -*-

(defvar +literate-patch/literate-config-file
  (concat doom-private-dir "config.org")
  "The file path of your literate config file.")

(defvar +literate-patch/literate-config-cache-file
  (concat doom-cache-dir "literate-last-compile")
  "The file path that `+literate-config-file' will be tangled to, then
byte-compiled from.")

(defvar +literate-patch/literate-tangle--async-proc nil)
(defvar +literate-patch/literate-tangle--async-proc-start-time nil)

(defvar org-mode-hook)
(defvar org-inhibit-startup)

;;;###autoload (add-hook 'org-mode-hook #'+literate-enable-recompile-h)

(defun +literate-patch/literate--guard-env-fn (category module)
  "Generate an env name to guard from re-running `org-babel-tangle-file'.

 For example: `(+literate-patch/literate--guard-env-fn 'config 'my-literate-module)'
 generates `__NOTANGLE_CONFIG_MY_LITERATE_MODULE'"
  (upcase
   (replace-regexp-in-string "-" "_"
                             (format "__NOTANGLE_%s_%s" category module))))

;;;###autoload
(defun +literate-patch/literate-tangle-module-h (category module)
  "Tangles `+literate-config-file' if it has changed.
This is performed with an asyncronous Emacs process, except when
`noninteractive' is non-nil."
  (let* ((literate-patch-dir (format "modules/%s/%s/" category module))
         ;; TODO support ofther literate files (e.g. README.org)
         (target (concat doom-private-dir literate-patch-dir "config.org"))
         ;; (dest "config.el")
         (dest (replace-regexp-in-string "\.org$" ".el" target))
         (cache (concat doom-cache-dir literate-patch-dir "literate-last-compile"))
         (guard-env (+literate-patch/literate--guard-env-fn category module)))
    (if noninteractive
        ; async doesn't seem to be working with my literate modules
        ;; (+literate-patch/literate-tangle--async target dest cache guard-env)
        (+literate-patch/literate-tangle--sync target dest cache guard-env)
      (+literate-patch/literate-tangle--sync target dest cache guard-env))
    ;; (+literate-patch/literate-tangle-fn target dest cache guard-env))
  ))

(defun +literate-patch/literate-tangle--async (target dest cache guard-env)
  "Tangles `+literate-config-file' using an async Emacs process."
  (unless (getenv guard-env)
    (let ((default-directory doom-private-dir)
          ;; (target +literate-config-file)
          ;; (cache +literate-config-cache-file)
          ;; (dest (expand-file-name (concat doom-module-config-file ".el")))
          )
      (when +literate-patch/literate-tangle--async-proc
        (message "Killing outdated tangle process...")
        (set-process-sentinel +literate-patch/literate-tangle--async-proc #'ignore)
        (kill-process +literate-patch/literate-tangle--async-proc)
        (sit-for 0.3)) ; ensure the message is seen for a bit
      (setq +literate-patch/literate-tangle--async-proc-start-time (float-time)
            +literate-patch/literate-tangle--async-proc
            ;; See `+literate-tangle--sync' for an explanation of the (progn ...) below.
            (start-process "tangle-config"
                           (get-buffer-create " *tangle config*")
                           "emacs" "--batch" "--eval"
                           (format "(progn \
(require 'ox) \
(require 'ob-tangle) \
(setq org-startup-indented nil \
      org-startup-folded nil \
      vc-handled-backends nil \
      write-file-functions nil \
      before-save-hook nil \
      after-save-hook nil \
      org-mode-hook nil \
      org-inhibit-startup t \
      org-confirm-babel-evaluate nil) \
(org-babel-tangle-file %S %S) \
(with-temp-file %S) \
)" target dest cache)))
      (set-process-sentinel +literate-patch/literate-tangle--async-proc #'+literate-patch/literate-tangle--async-sentinel)
      (run-at-time nil nil (lambda () (message "Tangling config.org"))) ; ensure shown after a save message
      "Tangling config.org...")))

(defun +literate-patch/literate-tangle--async-sentinel (process signal)
  (cond
   ((and (eq 'exit (process-status process))
         (= 0 (process-exit-status process)))
    (message "Tangled config.org sucessfully (took %.1fs)"
             (- (float-time) +literate-patch/literate-tangle--async-proc-start-time))
    (setq +literate-patch/literate-tangle--async-proc nil))
   ((memq (process-status process) (list 'exit 'signal))
    (pop-to-buffer (get-buffer " *tangle config*"))
    (message "Failed to tangle config.org (after %.1fs)"
             (- (float-time) +literate-patch/literate-tangle--async-proc-start-time))
    (setq +literate-patch/literate-tangle--async-proc nil))))

(defun +literate-patch/literate-tangle-check-finished ()
  "When a tangle is still in progress, ask the user if they want to wait for it."
  ;; (when (and (process-live-p +literate-patch/literate-tangle--async-proc)
  ;;            (yes-or-no-p "Config is currently retangling, would you please wait a few seconds?"))
  (when (process-live-p +literate-patch/literate-tangle--async-proc)
    (switch-to-buffer " *tangle config*")
    (signal 'quit nil)))

(add-hook! 'kill-emacs-hook #'+literate-patch/literate-tangle-check-finished)

(defun +literate-patch/literate-tangle--sync (target dest cache guard-env)
  "Tangles `+literate-config-file' if it has changed."
  (and (not (getenv guard-env))
       (require 'ox nil t)
       (require 'ob-tangle nil t)
       (letf! ((default-directory doom-private-dir)
               ;; (target +literate-config-file)
               ;; (cache +literate-config-cache-file)
               ;; (dest (expand-file-name (concat doom-module-config-file ".el")))
               ;; Ensure output conforms to the formatting of all doom CLIs
               (defun message (msg &rest args)
                 (when msg
                   (print! (item "%s") (apply #'format msg args)))))
         (print! (start "Compiling your literate config..."))
         ;;  make cache directory, if it doesn't already exist
         (let ((cache-dir (file-name-directory cache)))
            (unless (file-directory-p cache-dir) (make-directory cache-dir)))
         (print-group!
          (let (;; Do as little unnecessary work as possible in these org files.
                (org-startup-indented nil)
                (org-startup-folded nil)
                (vc-handled-backends nil)
                ;; Prevent unwanted entries in recentf, or formatters, or
                ;; anything that could be on these hooks, really. Nothing else
                ;; should be touching these files (particularly in interactive
                ;; sessions).
                (write-file-functions nil)
                (before-save-hook nil)
                (after-save-hook nil)
                ;; Prevent infinite recursion due to recompile-on-save hooks
                ;; later, and speed up `org-mode' init.
                (org-mode-hook nil)
                (org-inhibit-startup t)
                ;; Allow evaluation of src blocks at tangle-time (would abort
                ;; them otherwise). This is a security hazard, but Doom will
                ;; trust that you know what you're doing!
                (org-confirm-babel-evaluate nil))
            (org-babel-tangle-file target dest))
          ;; Write an empty file to serve as our mtime cache
          (with-temp-file cache)
          (if noninteractive t
            (print! "Restarting...")
            (exit! (format "__DOOMRESTART=1 %s=1 $@" guard-env)))))))

;;;###autoload
(defalias '+literate-patch/literate/reload #'doom/reload)

;;;###autoload
(defun +literate-patch/literate-enable-recompile-h ()
  "Enable literate-compiling-on-save in the current buffer."
  (add-hook 'after-save-hook #'+literate-patch/literate-recompile-maybe-h nil 'local))

;;;###autoload
(defun +literate-patch/literate-recompile-maybe-h ()
  "Recompile literate config to `doom-private-dir'.
We assume any org file in `doom-private-dir' is connected to your literate
config, and should trigger a recompile if changed."
  (and (file-in-directory-p
        buffer-file-name (file-name-directory +literate-config-file))
       (+literate-patch/literate-tangle-h)))
