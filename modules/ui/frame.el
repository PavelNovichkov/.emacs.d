;;; ui/frame.el -*- lexical-binding: t; -*-

(use-package frame ; built-in
  :straight nil

  :custom
  (blink-cursor-mode nil)
  (frame-resize-pixelwise t)
  (frame-title-format "Emacs")
  (menu-bar-mode nil)
  (scroll-bar-mode nil)
  (tool-bar-mode nil))

(use-package display-time ; built-in
  :straight nil

  :custom
  (display-time-default-load-average nil)
  (display-time-format "%a %d %b %R")

  :init
  (defun my/display-time-on-fullscreen ()
    "Display time when in fullscreen mode."
    (let ((fullscreen (frame-parameter nil 'fullscreen)))
      (if (memq fullscreen '(fullscreen fullboth))
          (display-time-mode 1)
        (display-time-mode -1))))
  (advice-add
   #'toggle-frame-fullscreen :after #'my/display-time-on-fullscreen))
