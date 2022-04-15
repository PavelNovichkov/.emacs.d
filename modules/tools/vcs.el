;;; tools/vcs.el -*- lexical-binding: t; -*-

(use-package magit
  :ensure-system-package git
  :commands magit-status
  :config
  (setq magit-section-visibility-indicator nil))

(use-package diff-hl
  :hook (((prog-mode conf-mode) . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (setq diff-hl-disable-on-remote t))
