;;; ui/frame.el -*- lexical-binding: t; -*-

;; Setup frame
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(modify-all-frames-parameters
 '((fullscreen . maximized)
   (internal-border-width . 15)
   (right-divider-width . 15)
   (bottom-divider-width . 15)))

(setq-default frame-resize-pixelwise t)

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode)
  (mu4e-view-mode . mixed-pitch-mode))

(defun my/setup-fonts ()
  "Setup fonts after GUI frame is created."
  (interactive)
  (set-face-attribute
   'default nil
   :family "Input Mono Narrow" :height 120 :weight 'normal)
  (set-face-attribute
   'fixed-pitch nil
   :family "Input Mono Narrow" :height 1.0 :weight 'normal)
  (set-face-attribute
   'variable-pitch nil
   :family "Input Sans Narrow" :height 1.0 :weight 'normal))
(add-hook 'server-after-make-frame-hook #'my/setup-fonts)
