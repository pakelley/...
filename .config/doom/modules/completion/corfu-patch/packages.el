;; -*- no-byte-compile: t; -*-
;;; completion/corfu-patch/packages.el

(package! corfu :recipe (:files ("*.el" "extensions/*.el")) :pin "a4f003605bc712952e5f2fc27d87730c312fdbeb")
(package! cape :pin "5b28cd43f2efa19dbf5053f164cce622a4b5bdae")
(when (modulep! +icons)
  (package! svg-lib :pin "b2a168482f85a308a4ab95c03e3c2bd71981e166")
  (package! kind-icon :pin "b0fb83be6ff6837446274e44e799ade836854a39"))
(when (modulep! +orderless)
  (package! orderless :pin "e6784026717a8a6a7dcd0bf31fd3414f148c542e"))
(when (modulep! :os tty)
  (package! corfu-terminal :pin "5ce4c11b8efd4d2fd1b404b9422bb85b05476da0"))
(when (modulep! :editor snippets)
  (package! yasnippet-capf :pin "40654214db7a44db3a99321447632b43a10fae57"))
