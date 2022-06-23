;;; layouts.el -*- lexical-binding: t; -*-

(use-package tab-bar ; built-in
  :straight nil
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-show 1)
  (tab-bar-format '(tab-bar-format-tabs tab-bar-format-align-right tab-bar-format-global))
  :init
  (setq tab-bar-new-tab-choice "*scratch*"
        tab-bar-new-tab-to 'rightmost))

(use-package rotate
  :commands (rotate-window rotate-layout))
