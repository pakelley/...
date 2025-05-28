;;; config/literate-patch/autoload.el -*- lexical-binding: t; -*-
;;;###autoload
(defun +literate-patch/literate-tangle-fn (target dest cache guard-env)
  "Tangles TARGET if it has changed"
  (and (not (getenv guard-env))
       (require 'ox nil t)
       (require 'ob-tangle nil t)
       (letf! (;; Ensure output conforms to the formatting of all doom CLIs
               (defun message (msg &rest args)
                 (when msg
                   (print! (info "%s") (apply #'format msg args)))))
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
          (if doom-interactive-p t
            (message "Restarting...")
            (throw 'exit (format "__DOOMRESTART=1 %s=1 $@" guard-env)))))))

; NOTE: just keeping this function for reference if I re-open my PR to doom after the cli rewrite
;;;###autoload
(defun +literate-patch/literate-tangle-h ()
  "Tangles `+literate-config-file' if it has changed."
       (letf! ((default-directory doom-private-dir)
               (target +literate-config-file)
               (dest (expand-file-name (concat doom-module-config-file ".el")))
               (cache +literate-config-cache-file))
         (+literate-patch/literate-tangle-fn target dest cache "__NOTANGLE")))

(defun +literate-patch/literate--guard-env-fn (category module)
  "Generate an env name to guard from re-running `org-babel-tangle-file'.

 For example: `(+literate-patch/literate--guard-env-fn 'config 'my-literate-module)'
 generates `__NOTANGLE_CONFIG_MY_LITERATE_MODULE'"
  (upcase
   (replace-regexp-in-string "-" "_"
                             (format "__NOTANGLE_%s_%s" category module))))


;;;###autoload
(defun +literate-patch/literate-tangle-module-h (category module)
  "Tangle `config.org' for the module specified by CATEGORY and MODULE."
  (let* ((literate-patch-dir (format "modules/%s/%s/" category module))
         ;; TODO support ofther literate files (e.g. README.org)
         (target (concat doom-private-dir literate-patch-dir "config.org"))
         (dest "config.el")
         (cache (concat doom-cache-dir literate-patch-dir "literate-last-compile"))
         (guard-env (+literate-patch/literate--guard-env-fn category module)))
    (+literate-patch/literate-tangle-fn target dest cache guard-env)))
