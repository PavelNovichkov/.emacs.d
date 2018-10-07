;;; core-evil.el --- Evil setup -*- lexical-binding: t; -*-

;;; Evil.
(use-package evil
  :demand
  :init
  (setq evil-want-C-d-scroll t
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-want-Y-yank-to-eol t
        evil-want-integration nil
        evil-mode-line-format nil
        evil-symbol-word-search t
        evil-respect-visual-line-mode t
        ;; search and replace
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-search-module 'evil-search
        ;; selections
        evil-ex-visual-char-range t
        evil-want-visual-char-semi-exclusive t
        evil-visual-state-cursor 'hollow
        )
  :config
  (evil-mode)
  (general-define-key :states 'normal "<escape>" #'evil-ex-nohighlight))

(provide 'core-evil)
;;; core-evil.el ends here
