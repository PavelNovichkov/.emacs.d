;;; layouts.el -*- lexical-binding: t; -*-

(use-package eyebrowse
  :commands eyebrowse-create-window-config
  :init
  (setq eyebrowse-mode-map nil) ; Do not assign default key bindings
  :config
  (setq eyebrowse-new-workspace t) ; Start workspace with *scratch* buffer
  (setq eyebrowse-wrap-around t)
  (eyebrowse-mode t))

(use-package rotate
  :commands (rotate-window rotate-layout))
