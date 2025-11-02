;;; emacs/eldoc.el -*- lexical-binding: t; -*-

(use-package eldoc ; built-in
  :ensure nil
  :config
  (setq eldoc-echo-area-display-truncation-message nil
        eldoc-echo-area-prefer-doc-buffer t
        eldoc-echo-area-use-multiline-p nil))
