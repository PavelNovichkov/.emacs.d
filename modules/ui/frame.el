;;; ui/frame.el -*- lexical-binding: t; -*-

(use-package frame ; built-in
  :straight nil

  :custom
  (blink-cursor-mode nil)
  (frame-resize-pixelwise t)
  (frame-title-format "Emacs")
  (menu-bar-mode nil)
  (scroll-bar-mode nil)
  (tool-bar-mode nil)

  :config
  (modify-all-frames-parameters
   '((fullscreen . maximized)
     (internal-border-width . 15)
     (right-divider-width . 15)
     (bottom-divider-width . 15))))

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

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode)
  (mu4e-view-mode . mixed-pitch-mode))

(defun my/setup-fonts ()
  "Setup fonts after GUI frame is created."
  (interactive)
  (set-face-attribute
   'default nil :family "Input Mono Narrow" :height 110 :weight 'normal)
  (set-face-attribute
   'fixed-pitch nil :family "Input Mono Narrow" :height 1.0 :weight 'normal)
  (set-face-attribute
   'variable-pitch nil :family "Input Sans Narrow" :height 1.0 :weight 'normal))

(add-hook 'server-after-make-frame-hook #'my/setup-fonts)
