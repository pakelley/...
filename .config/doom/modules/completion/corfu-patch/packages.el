;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(package! corfu)
(when (modulep! +orderless)
  (package! orderless))
(package! kind-icon)
(package! cape :recipe (:host github :repo "minad/cape" :branch "main"))
(package! corfu-doc :recipe (:host github :repo "galeo/corfu-doc" :branch "main"))
