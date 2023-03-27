(use-package! kubernetes
  :defer t
  :commands (kubernetes-overview k8s)
  :config
  (fset 'k8s 'kubernetes-overview)
)
(use-package! kubernetes-evil
  :defer t
  :after kubernetes)
