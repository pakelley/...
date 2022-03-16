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

(setq-default evil-kill-on-visual-paste nil)
