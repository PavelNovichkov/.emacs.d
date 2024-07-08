;;; ui/hl-line.el -*- lexical-binding: t; -*-

(use-package hl-line ; built-in
  :straight nil

  :hook
  ((prog-mode org-agenda-mode dired-mode) . hl-line-mode))
