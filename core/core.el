;;; core.el --- Configuration core -*- lexical-binding: t; -*-

;;; Optimize garbage collection.

(defun my/increase-gc-threshold ()
  (setq gc-cons-threshold 262144000)) ; 250 Mb.

(defun my/restore-gc-threshold ()
  (setq gc-cons-threshold 16777216)) ; 16 Mb.

(if (or after-init-time noninteractive)
    (my/restore-gc-threshold)
  (my/increase-gc-threshold)
  (add-hook 'emacs-startup-hook #'my/restore-gc-threshold))

(add-hook 'minibuffer-setup-hook #'my/increase-gc-threshold)
(add-hook 'minibuffer-exit-hook #'my/restore-gc-threshold)

;;; Load core modules.

(dolist (module-name '("core-packages"
                       "core-keybindings"
                       "core-emacs"
                       "core-evil"
                       "core-completion"
                       "core-ui"))
  (load (concat user-emacs-directory "core/" module-name) nil t))
