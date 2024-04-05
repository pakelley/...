(use-package! kubernetes
  :defer t
  :commands (kubernetes-overview k8s)
  :config
  (fset 'k8s 'kubernetes-overview)
)
(use-package! kubernetes-evil
  :defer t
  :after kubernetes)
(use-package! kele
  :config
  (kele-mode 1)
  (bind-key (kbd "s-k") kele-command-map kele-mode-map))
