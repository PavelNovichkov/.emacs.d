;;; ui/fonts.el -*- lexical-binding: t; -*-

;;; Default fonts

(set-face-attribute
 'default nil :family "Input Mono Narrow" :height 110 :weight 'normal)
(set-face-attribute
 'fixed-pitch nil :family "Input Mono Narrow" :height 1.0 :weight 'normal)
(set-face-attribute
 'variable-pitch nil :family "Input Sans Narrow" :height 1.0 :weight 'normal)
(set-face-attribute 'variable-pitch-text nil :height 1.0)

;; Set font for missing characters (Greek and all characters after Cyrillic).

(defconst my/missing-chars
  '(greek (#x0530 . #x10FFFF))
  "List of character ranges to override.")

(dolist (chars my/missing-chars)
  (set-fontset-font t chars (font-spec :family "JuliaMono") nil 'prepend))


;;; Mixed pitch mode

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode)
  (Info-mode . mixed-pitch-mode)
  (mu4e-view-mode . mixed-pitch-mode))
