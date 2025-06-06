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

(use-package! persp-mode
  :commands persp-add-buffer
  :config
  (map! :leader
        (:prefix ("TAB" . "workspace")
         :desc "Add buffer to a workspace" "a" #'persp-add-buffer)))

(map! "s-\\" #'+workspace/switch-to)

(map!
 (:map evil-eval-map "C-k" nil)
 (:map evil-insert-state-map "C-k" nil)
 (:map evil-replace-state-map "C-k" nil)
 (:map read-expression-map "C-k" nil)
 (:map vertico-map "C-k" nil)
 (:map vertico-map :i "C-k" nil))

(fset 'epg-wait-for-status 'ignore)

(setq epa-file-encrypt-to '("pakelley@hey.com"))
(setq epa-file-encrypt-to-key '("pakelley@hey.com"))
(setq epa-file-select-keys nil) ;; make sure we don't use symmetric encryption
(setq plstore-encrypt-to '("pakelley@hey.com"))

;; (setq auth-sources (push "~/.authinfo.gpg" auth-sources))
;; previously: (macos-keychain-generic macos-keychain-internet "~/.config/doom-emacs/.local/state/authinfo.gpg" "~/.authinfo.gpg")
(use-package auth-source)
(auth-source-forget-all-cached)
(setq auth-sources '("~/.authinfo.gpg"))

(setq epa-pinentry-mode 'ask)
(setq epg-pinentry-mode 'ask)

(use-package! age
  :demand t
  :custom
  (age-program "rage")
  (age-default-identity "~/.local/share/private/age/yubikey-main-identity.txt")
  (age-default-recipient
   '("~/.local/share/private/age/yubikey-main-recipient.txt"
     "~/.local/share/private/age/yubikey-backup-recipient.txt"))
  (age-pinentry-mode 'ask)
  :config
  (age-file-enable))
(use-package! pinentry
  :config
  (pinentry-start))

(use-package! chezmoi)
