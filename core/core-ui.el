;;; core-ui.el --- User interface setup -*- lexical-binding: t; -*-

;;; Anzu

(use-package anzu
  :demand
  :config
  (setq anzu-search-threshold 1000
        anzu-replace-threshold 1000)
  (global-anzu-mode 1))

(use-package evil-anzu
  :demand
  :after (evil anzu))

;;; Modeline

(use-package doom-modeline
  :hook (after-init . doom-modeline-init)
  :config
  (setq
   ;; Don't include project name in the file name as it is very slow.
   doom-modeline-buffer-file-name-style 'file-name
   ;; Make sure icon is displayed
   doom-modeline-icon t
   doom-modeline-workspace-name nil))

;;; Posframe

(use-package posframe
  :demand :if window-system
  :config
  (setq posframe-mouse-banish nil))

;;; All the icons

(use-package all-the-icons
  :config
  (setq all-the-icons-scale-factor 1.0))
