;;; lang/cc.el -*- lexical-binding: t; -*-

(use-package cc-mode ; built-in
  :straight nil
  :config
  ;; Bindings.
  (local-leader-def
    :keymaps 'c-mode-map
    "t" '("toggle" . (keymap))
    "tc" '("comment style" . c-toggle-comment-style)
    "te" '("electricity" . c-toggle-electric-state)))
