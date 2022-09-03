;;; ui/color-theme.el -*- lexical-binding: t; -*-

(use-package modus-themes
  :demand
  :config
  (setq modus-themes-diffs 'bg-only
        modus-themes-lang-checkers '(text-also straight-underline)
        modus-themes-region '(bg-only no-extend)
        modus-themes-subtle-line-numbers t)
  (load-theme 'modus-operandi t))
