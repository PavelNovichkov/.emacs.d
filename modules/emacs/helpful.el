;;; emacs/helpful.el -*- lexical-binding: t; -*-

(use-package helpful
  :commands (helpful-at-point
             helpful-callable
             helpful-key
             helpful-symbol
             helpful-variable)
  :config
  (add-to-list
   'display-buffer-alist
   '("\\*helpful.*"
     (display-buffer-reuse-mode-window display-buffer-in-side-window)
     (side . right)
     (window-width . 80))))
