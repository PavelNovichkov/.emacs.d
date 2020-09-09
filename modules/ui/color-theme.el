;;; ui/color-theme.el -*- lexical-binding: t; -*-

(use-package modus-operandi-theme
  :demand
  :config
  (setq modus-operandi-theme-completions 'opinionated)
  (load-theme 'modus-operandi t))
