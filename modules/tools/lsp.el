;;; lang/lsp.el -*- lexical-binding: t; -*-

(use-package eglot
  :commands (eglot eglot-ensure)
  :config
  (setq eglot-autoshutdown t))

(use-package consult-eglot
  :demand :after eglot)
