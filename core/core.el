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

(require 'core-packages (concat user-emacs-directory "core/core-packages.el"))
(require 'core-keybindings (concat user-emacs-directory "core/core-keybindings.el"))
(require 'core-emacs (concat user-emacs-directory "core/core-emacs.el"))
(require 'core-evil (concat user-emacs-directory "core/core-evil.el"))
(require 'core-completion (concat user-emacs-directory "core/core-completion.el"))
(require 'core-ui (concat user-emacs-directory "core/core-ui.el"))

(provide 'core)
;;; core.el ends here
