;;; core-ui.el --- User interface setup -*- lexical-binding: t; -*-

;;; Cursor

(blink-cursor-mode -1)

;;; Modeline

(use-package nano-modeline
  :hook (after-init . nano-modeline-mode))

;;; Posframe

(use-package posframe
  :demand)

;;; All the icons

(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1.0))

(use-package all-the-icons-completion
  :demand
  :custom
  (all-the-icons-completion-mode t))
