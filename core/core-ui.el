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
  :hook (after-init . doom-modeline-init))

(provide 'core-ui)
;;; core-ui.el ends here
