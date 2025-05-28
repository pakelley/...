(package! numpydoc)

(package! blacken)

(package! flymake-ruff
  :recipe (:type git :host github :repo "erickgnavar/flymake-ruff"))

(package! reformatter)

(package! cov)

;; required for msgu, a dep of ts-docstr
;; (add-to-list 'package-archives '( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)
;; (package! msgu :recipe (:host github :repo "jcs-elpa/msgu"))
;; (package! ts-docstr
;;   :recipe (:host github :repo "emacs-vs/ts-docstr" :files (:defaults "langs/*.el")))
