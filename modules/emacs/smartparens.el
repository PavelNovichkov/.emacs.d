;;; emacs/smartparens.el -*- lexical-binding: t; -*-

(use-package smartparens-config
  :ensure smartparens
  :demand
  :config
  (setq sp-message-width nil)
  (smartparens-global-mode)
  (show-smartparens-global-mode))

;; TODO: Add evil keybindings for insert state.
