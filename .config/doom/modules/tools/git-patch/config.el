(use-package! magit
  :defer t
  :commands magit-status
  :config
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
  (remove-hook 'magit-status-headers-hook 'magit-insert-tags-header)
  (magit-add-section-hook
   'magit-status-sections-hook
   'magit-insert-unpushed-cherries
   'magit-insert-unpushed-to-upstream-or-recent
   'replace)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream))

(use-package! magit-arcanist
  :defer t
  :after magit
  :ghook ('magit-mode-hook #'magit-arcanist-enable)
  :custom
  (magit-arcanist-key (kbd "#")))
