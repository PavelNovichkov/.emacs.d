;;; emacs/word-wrap.el -*- lexical-binding: t; -*-

(use-package emacs
  :straight nil

  :config
  (setopt fill-column 80)
  (global-visual-line-mode))


(use-package visual-wrap ; built-in
  :straight nil

  :hook
  (prog-mode . visual-wrap-prefix-mode)
  (org-agenda-mode . visual-wrap-prefix-mode)

  :config
  (setopt visual-wrap-extra-indent 2))
