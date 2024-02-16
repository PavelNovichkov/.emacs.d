;;; ui/evil-goggles.el -*- lexical-binding: t; -*-

(use-package evil-goggles
  :demand :after evil

  :config
  (setq evil-goggles-pulse t
        evil-goggles-enable-change nil
        evil-goggles-enable-commentary nil
        evil-goggles-enable-delete nil
        evil-goggles-enable-indent nil
        evil-goggles-enable-surround nil)
  (evil-goggles-mode))
