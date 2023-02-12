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

(defconst my/fonts '("Fira Code Retina-12" . "Source Serif 4-15"))

(defun my/setup-fonts ()
  "Setup fonts after GUI frame is created."
  (set-face-attribute 'default nil :font (car my/fonts))
  (set-face-attribute 'fixed-pitch nil :font (car my/fonts))
  (set-face-attribute 'variable-pitch nil :font (cdr my/fonts)))
(add-hook 'server-after-make-frame-hook #'my/setup-fonts)
