;;; ui/ivy-rich.el -*- lexical-binding: t; -*-

(use-package ivy-rich
  :disabled
  :demand :after ivy
  :config
  (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :disabled
  :demand :after ivy-rich
  :config
  (all-the-icons-ivy-rich-mode 1))
