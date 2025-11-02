;;; layouts.el -*- lexical-binding: t; -*-

(use-package tab-bar ; built-in
  :ensure nil

  :custom
  (tab-bar-auto-width nil)
  (tab-bar-close-button-show nil)
  (tab-bar-format
   '(tab-bar-format-tabs tab-bar-format-align-right tab-bar-format-global))
  (tab-bar-mode t)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-new-tab-to 'rightmost))

(use-package rotate
  :commands (rotate-window rotate-layout))
