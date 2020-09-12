;;; emacs/ibuffer.el -*- lexical-binding: t; -*-

(use-package ibuffer ; built-in
  :straight nil
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :config
  (setq ibuffer-show-empty-filter-groups nil))

(use-package ibuffer-projectile
  :hook (ibuffer . ibuffer-projectile-set-filter-groups)
  :config
  (setq ibuffer-projectile-prefix ""))
