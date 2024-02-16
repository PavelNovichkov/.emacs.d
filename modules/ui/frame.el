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

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode)
  (mu4e-view-mode . mixed-pitch-mode))

(defun my/setup-fonts ()
  "Setup fonts after GUI frame is created."
  (interactive)
  (set-face-attribute
   'default nil :family "Input Mono" :height 110 :weight 'normal)
  (set-face-attribute
   'fixed-pitch nil :family "Input Mono" :height 1.0 :weight 'normal)
  (set-face-attribute
   'variable-pitch nil :family "Input Sans" :height 1.0 :weight 'normal))

(add-hook 'server-after-make-frame-hook #'my/setup-fonts)
