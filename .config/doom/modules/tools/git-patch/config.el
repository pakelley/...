(use-package! code-review
  :after forge
  :config
  (define-key magit-mode-map "R" #'code-review-forge-pr-at-point))

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

(use-package! git-link
  :after git-gutter-mode
  :general (:keymaps 'doom-leader-git-map
            "<left>" 'git-link))
