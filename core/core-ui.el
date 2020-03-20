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
  ; Don't include project name in the file name as it is very slow.
  (setq doom-modeline-buffer-file-name-style 'file-name))

;;; Posframe

(use-package posframe
  :demand :if window-system
  :config
  (setq posframe-mouse-banish nil))

(provide 'core-ui)
;;; core-ui.el ends here
