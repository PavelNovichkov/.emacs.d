;;; ui/hl-line.el -*- lexical-binding: t; -*-

(use-package hl-line ; built-in
  :straight nil

  :hook
  (prog-mode . hl-line-mode))


(use-package lin
  :demand

  :config
  (setopt lin-face 'completions-highlight)
  (lin-global-mode))
