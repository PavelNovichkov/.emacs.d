;;; lang/elisp.el -*- lexical-binding: t; -*-

(use-package elisp-mode ; built-in
  :straight nil
  :config
  ;; Bindings.
  (local-leader-def
    :keymaps 'emacs-lisp-mode-map
    "e" '(eval-last-sexp :which-key "evaluate expression")
    "x" '(eval-defun :which-key "evaluate function")))
