;;; ui/windows.el -*- lexical-binding: t; -*-

(use-package window ; built-in
  :ensure nil

  :custom
  (hscroll-step 1)
  (scroll-conservatively 101)
  (scroll-margin 0)
  (scroll-preserve-screen-position t)
  (switch-to-buffer-in-dedicated-window 'pop)
  (switch-to-buffer-obey-display-actions t)
  (window-resize-pixelwise t)
  (winner-dont-bind-my-keys t)
  (winner-mode t))

(use-package pixel-scroll ; built-in
  :ensure nil

  :custom
  (pixel-scroll-precision-mode t))
