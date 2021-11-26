;;; ui/frame.el -*- lexical-binding: t; -*-

;; Setup frame
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(defconst my/fonts '("Fira Code Retina-12" . "Source Serif Pro-15"))

(defun my/setup-fonts (frame)
  "Setup fonts after GUI frame is created."
  (set-face-attribute 'default frame :font (car my/fonts))
  (set-face-attribute 'fixed-pitch frame :font (car my/fonts))
  (set-face-attribute 'variable-pitch frame :font (cdr my/fonts)))
(add-hook 'after-make-frame-functions #'my/setup-fonts)
