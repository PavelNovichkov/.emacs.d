;;; tools/magit.el -*- lexical-binding: t; -*-

(use-package magit
  :commands magit-status
  :config
  (setq magit-section-visibility-indicator nil))

(use-package evil-magit
  :demand :after magit
  :init
  (setq evil-magit-use-z-for-folds t
        evil-magit-want-horizontal-movement t)
  :config
  (general-define-key
   :states evil-magit-state
   :keymaps 'magit-mode-map
   "?" #'evil-search-backward
   "zz" #'evil-scroll-line-to-center
   "zt" #'evil-scroll-line-to-top
   "zb" #'evil-scroll-line-to-bottom))
