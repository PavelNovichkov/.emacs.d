;;; lang/elisp.el -*- lexical-binding: t; -*-

(use-package elisp-mode ; built-in
  :straight nil
  :config
  ;; Bindings.
  (local-leader-def
    :keymaps 'emacs-lisp-mode-map
    "e" '("evaluate expression" . eval-last-sexp)
    "x" '("evaluate function" . eval-defun)))
