;;; emacs/word-wrap.el -*- lexical-binding: t; -*-

(use-package emacs
  :straight nil

  :config
  (setopt fill-column 80)
  (global-visual-line-mode))


(use-package adaptive-wrap
  :hook
  (prog-mode . adaptive-wrap-prefix-mode)
  (org-agenda-mode . adaptive-wrap-prefix-mode)

  :config
  (setopt adaptive-wrap-extra-indent 2))
