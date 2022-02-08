;;; tools/dictionary.el -*- lexical-binding: t; -*-

(use-package dictionary
  :ensure-system-package dictd
  :config
  (setq dictionary-server "localhost"
        dictionary-use-single-buffer t))
