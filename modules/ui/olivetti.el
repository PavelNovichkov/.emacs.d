;;; ui/olivetti.el -*- lexical-binding: t; -*-

(use-package olivetti
  :hook
  ((text-mode prog-mode) . olivetti-mode)

  :config
  (setq-default olivetti-body-width 80))
