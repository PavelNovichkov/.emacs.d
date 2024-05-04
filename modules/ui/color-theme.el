;;; ui/color-theme.el -*- lexical-binding: t; -*-

(use-package nano-theme
  :demand

  :custom
  (nano-light-popout "#DC711A"))

(use-package auto-dark
  :custom
  (auto-dark-light-theme 'nano-light)
  (auto-dark-dark-theme 'nano-dark)
  (auto-dark-mode t))
