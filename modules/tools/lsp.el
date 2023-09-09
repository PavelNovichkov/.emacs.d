;;; tools/lsp.el -*- lexical-binding: t; -*-

(use-package eglot ; built-in
  :straight nil
  :commands (eglot eglot-ensure)
  :config
  (setq eglot-autoshutdown t))

(use-package consult-eglot
  :demand :after eglot)
