;;; emacs/recentf.el -*- lexical-binding: t; -*-

(use-package recentf ; built-in
  :ensure nil
  :demand
  :config
  (setq recentf-filename-handlers '(file-truename abbreviate-file-name)
        recentf-max-saved-items 30)
  ;; Avoid hanging when remote host is unreachable, see e.g.
  ;; https://github.com/hlissner/doom-emacs/issues/1444.
  (setopt remote-file-name-access-timeout 3)
  (defun my/recentf-add-dired-directory ()
    "Add dired directory to recentf file list."
    (recentf-add-file default-directory))
  (add-hook 'dired-mode-hook #'my/recentf-add-dired-directory)
  (recentf-mode))
