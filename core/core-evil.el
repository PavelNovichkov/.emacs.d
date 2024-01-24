;;; core-evil.el --- Evil setup -*- lexical-binding: t; -*-

;;; Evil.
(use-package evil
  :demand
  :custom
  (evil-want-C-d-scroll t)
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-keybinding nil)
  (evil-want-integration t)
  (evil-mode-line-format nil)
  (evil-symbol-word-search t)
  (evil-respect-visual-line-mode t)
  (evil-undo-system 'undo-redo)
  ;; search and replace
  (evil-ex-search-vim-style-regexp t)
  (evil-ex-substitute-global t)
  (evil-search-module 'evil-search)
  ;; selections
  (evil-ex-visual-char-range t)
  (evil-v$-excludes-newline t)
  (evil-visual-state-cursor 'hollow)
  ;; this does not work:
  ;; (evil-mode t)
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
  (add-hook 'my/escape-hook #'my/disable-ex-highlights))

;;; Evil collection

(use-package evil-collection
  :demand :after evil
  :init
  (setq evil-collection-setup-minibuffer t
        evil-collection-want-unimpaired-p nil)
  :config
  (setq evil-collection-key-blacklist
        (list leader-key local-leader-key))
  (evil-collection-init))
