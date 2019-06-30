;;; emacs/tramp.el -*- lexical-binding: t; -*-

(use-package tramp ; built-in
  :config
  ;; Do not create .tramp_history files on remote machines.
  (setq tramp-histfile-override t))
