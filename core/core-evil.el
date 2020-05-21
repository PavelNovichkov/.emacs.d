;;; core-evil.el --- Evil setup -*- lexical-binding: t; -*-

;;; Evil.
(use-package evil
  :demand
  :init
  (setq evil-want-C-d-scroll t
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-want-Y-yank-to-eol t
        evil-want-keybinding nil
        evil-want-integration t
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
  ;; Make ESC from normal mode the universal escaper.
  (defun my/evil-normal-state-escape (&rest _)
  "Call `my/escape' if `evil-force-normal-state' is called interactively."
  (when (called-interactively-p 'any)
    (call-interactively #'my/escape)))
  (advice-add #'evil-force-normal-state :after #'my/evil-normal-state-escape)
  ;; Disable highlights on escape.
  (defun my/disable-ex-highlights ()
      "Disable ex search buffer highlights."
      (when (evil-ex-hl-active-p 'evil-ex-search)
        (evil-ex-nohighlight)
        t))
  (add-hook 'my/escape-hook #'my/disable-ex-highlights)
  ;; Disable undo-tree in favor of undo-fu.
  (global-undo-tree-mode -1)
  (general-define-key
   :states 'normal
   "u" #'undo-fu-only-undo
   "C-r" #'undo-fu-only-redo))

(provide 'core-evil)
;;; core-evil.el ends here
