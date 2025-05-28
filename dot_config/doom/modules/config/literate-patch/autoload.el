;;; config/literate-patch/new-autoload.el -*- lexical-binding: t; -*-

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
         (guard-env (+literate-patch/literate--guard-env-fn category module)))
    (if noninteractive
        (+literate-patch/literate-tangle--sync target dest guard-env)
      (+literate-patch/literate-tangle--async target dest guard-env))))


(defun +literate-patch/literate-tangle--async (target dest guard-env)
  "Tangles `+literate-config-file' using an async Emacs process."
  (unless (getenv guard-env)
    (when +literate-tangle--async-proc
      (message "Killing outdated tangle process...")
      (set-process-sentinel +literate-tangle--async-proc #'ignore)
      (kill-process +literate-tangle--async-proc)
      (sit-for 0.3)) ; ensure the message is seen for a bit
    (setq +literate-tangle--async-proc-start-time (float-time)
          +literate-tangle--async-proc
          ;; See `+literate-tangle--sync' for an explanation of the (progn ...) below.
          (start-process "tangle-config"
                         (get-buffer-create " *tangle config*")
                         "emacs" "--batch"
                         "--eval"
                         (prin1-to-string
                          `(progn
                             (require 'cl-lib)
                             (require 'subr-x)
                             (load ,(doom-path doom-core-dir "autoload/print"))
                             (funcall #',(symbol-function #'+literate-tangle)
                                    ,target
                                    ,dest
                                    ,doom-private-dir)))))
    (add-hook 'kill-emacs-hook #'+literate-tangle-check-finished-h)
    (set-process-sentinel +literate-tangle--async-proc #'+literate-tangle--async-sentinel)
    (run-at-time nil nil (lambda () (message "Tangling config.org"))) ; ensure shown after a save message
    "Tangling config.org..."))

(defun +literate-patch/literate-tangle--sync (target dest guard-env)
  "Tangles `+literate-config-file' if it has changed."
  (and (not (getenv guard-env))
       (+literate-tangle target
                         dest
                         doom-private-dir)
       (always (print! "Restarting..."))
       (exit! (format "__DOOMRESTART=1 %s=1 $@" guard-env))))
