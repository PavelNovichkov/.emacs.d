;;; ui/frame.el -*- lexical-binding: t; -*-

;; Setup frame
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(defconst my/fonts '("Fira Code Retina-12" . "Source Serif Pro-15"))

(defun my/setup-fonts ()
  "Setup fonts after GUI frame is created."
  (set-face-attribute 'default nil :font (car my/fonts))
  (set-face-attribute 'fixed-pitch nil :font (car my/fonts))
  (set-face-attribute 'variable-pitch nil :font (cdr my/fonts)))
(add-hook 'server-after-make-frame-hook #'my/setup-fonts)
