;;; emacs/eldoc.el -*- lexical-binding: t; -*-

(use-package eldoc ; built-in
  :ensure nil
  :config
  (setq eldoc-echo-area-display-truncation-message nil
        eldoc-echo-area-prefer-doc-buffer t
        eldoc-echo-area-use-multiline-p nil)
  (add-to-list
   'display-buffer-alist
   '("^\\*eldoc\\*"
     (display-buffer-in-side-window)
     (side . right)
     (slot . 0)
     (window-width . 80))))
