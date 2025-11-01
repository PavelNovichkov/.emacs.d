;;; ui/fonts.el -*- lexical-binding: t; -*-

;;; Default fonts

(set-face-attribute
 'default nil :family "Input Sans Narrow" :height 100 :weight 'normal)
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


;;; Fixed pitch mode

(define-minor-mode fixed-pitch-mode
  "Fixed-pitch default-face mode."
  :init-value nil
  (if fixed-pitch-mode
      (buffer-face-set 'fixed-pitch)
    (buffer-face-set)))

(defconst my/fixed-pitch-hooks
  '(calendar-mode-hook
    comint-mode-hook
    dired-mode-hook
    ibuffer-mode-hook
    ledger-report-mode-hook
    magit-mode-hook
    minibuffer-setup-hook
    org-agenda-mode-hook
    vterm-mode-hook
    which-key-init-buffer-hook)
  "List of hooks that should activate fixed-pitch font.")

(dolist (hook my/fixed-pitch-hooks) (add-hook hook #'fixed-pitch-mode))
