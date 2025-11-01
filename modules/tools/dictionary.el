;;; tools/dictionary.el -*- lexical-binding: t; -*-

(use-package dictionary ; built-in
  :straight nil
  :ensure-system-package dictd

  :config
  (setopt
   dictionary-server "localhost"
   dictionary-use-single-buffer t))
