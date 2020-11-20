;;; layouts.el -*- lexical-binding: t; -*-

(use-package tab-bar ; built-in
  :straight nil
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-show 1)
  :init
  (setq tab-bar-new-tab-choice "*scratch*"
        tab-bar-new-tab-to 'rightmost))

(use-package rotate
  :commands (rotate-window rotate-layout))
