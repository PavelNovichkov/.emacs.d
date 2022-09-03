;;; lang/cc.el -*- lexical-binding: t; -*-

(use-package cc-mode ; built-in
  :straight nil
  :config
  ;; Bindings.
  (local-leader-def
    :keymaps 'c-mode-map
    "t" '(:ignore t :which-key "toggle")
    "tc" '(c-toggle-comment-style :which-key "comment style")))
