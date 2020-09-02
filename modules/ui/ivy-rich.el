;;; ui/ivy-rich.el -*- lexical-binding: t; -*-

(use-package ivy-rich
  :demand :after ivy
  :config
  (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :demand :after ivy-rich
  :config
  (all-the-icons-ivy-rich-mode 1))
