;;; tools/magit.el -*- lexical-binding: t; -*-

(use-package magit
  :ensure-system-package git
  :commands magit-status
  :config
  (setq magit-section-visibility-indicator nil))
