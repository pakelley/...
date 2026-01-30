;;; config/literate-patch/autoload.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun +literate-patch--module-orgs ()
  "Return a list of (CATEGORY MODULE ORG DEST) under `doom-private-dir'."
  (let ((root (file-name-concat doom-private-dir "modules")))
    (when (file-directory-p root)
      (cl-loop for category in (directory-files root nil directory-files-no-dot-files-regexp)
               for category-path = (file-name-concat root category)
               if (file-directory-p category-path)
               nconc
               (cl-loop for module in (directory-files category-path nil directory-files-no-dot-files-regexp)
                        for module-path = (file-name-concat category-path module)
                        for org-path = (file-name-concat module-path "config.org")
                        for dest-path = (file-name-concat module-path "config.el")
                        if (file-exists-p org-path)
                        collect (list category module org-path dest-path))))))

;;;###autoload
(defun +literate-patch/tangle-all-h ()
  "Tangle every literate private module before `doom sync'."
  (unless (getenv "__NOTANGLE") ; respect restart guard used by Doom's own literate module
    (dolist (entry (+literate-patch--module-orgs))
      (pcase-let ((`(,category ,module ,org-path ,dest-path) entry))
        (print! (start "Tangling %s/%s...") category module)
        (+literate-tangle org-path dest-path doom-private-dir)))))

(defvar +literate-patch--tangle-hook-installed nil
  "Internal flag to avoid installing `+literate-patch/tangle-all-h' twice.")

(unless +literate-patch--tangle-hook-installed
  (add-hook 'doom-before-sync-hook #'+literate-patch/tangle-all-h)
  (setq +literate-patch--tangle-hook-installed t))
