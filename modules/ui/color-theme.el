;;; ui/color-theme.el -*- lexical-binding: t; -*-

(use-package modus-themes
  :demand
  :config
  (setq modus-themes-completions 'opinionated
        modus-themes-diffs 'bg-only
        modus-themes-region 'bg-only-no-extend)
  (load-theme 'modus-operandi t))
