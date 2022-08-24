(setq doom-theme 'doom-oceanic-next)

(setq evil-snipe-override-evil-repeat-keys nil)
(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-,")

(map! (:leader
       :desc "M-x"  "SPC" #'execute-extended-command))

(map! (:map evil-window-map
       "/" #'+evil-window-vsplit-a
       "?" #'+evil/window-vsplit-and-follow
       "-" #'+evil-window-split-a
       "_" #'+evil/window-split-and-follow))

(setq display-line-numbers-type 'relative)

(after! doom-modeline
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project))

(setq-default evil-kill-on-visual-paste nil)
