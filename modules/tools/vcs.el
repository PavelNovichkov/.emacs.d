;;; tools/vcs.el -*- lexical-binding: t; -*-

;; Magit requires a newer version of transient than the one included with Emacs,
;; so we have to install it explicitly.
(use-package transient)

(use-package magit
  :ensure-system-package git
  :commands magit-status
  :custom
  (magit-display-buffer-function #'display-buffer)
  (magit-section-visibility-indicator nil)
  :config
  (add-to-list
   'display-buffer-alist
   '("magit.*"
     (display-buffer-in-side-window)
     (side . right)
     (slot . 0)
     (window-width . 80)))
  (add-to-list
   'display-buffer-alist
   '("COMMIT_EDITMSG"
     (display-buffer-in-side-window)
     (side . right)
     (slot . 1)
     (window-width . 80)
     (window-height . 10))))

(use-package diff-hl
  :hook (((prog-mode conf-mode) . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :custom
  (diff-hl-disable-on-remote t)
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
