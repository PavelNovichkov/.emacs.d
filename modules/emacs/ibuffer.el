;;; emacs/ibuffer.el -*- lexical-binding: t; -*-

(use-package ibuffer ; built-in
  :config
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("dired" (mode . dired-mode))
           ("org" (or (mode . org-mode) (name . "^Org Agenda$")))
           ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
           ("mu4e" (name . "\*mu4e\*"))
           ("programming" (mode . python-mode))
           ("emacs" (or
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Messages\\*$")))
           )))
  (defun my/ibuffer-setup ()
    (ibuffer-auto-mode 1)
    (ibuffer-switch-to-saved-filter-groups "default"))
  (add-hook 'ibuffer-mode-hook #'my/ibuffer-setup)
  (setq ibuffer-show-empty-filter-groups nil))
