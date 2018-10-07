;;; core.el --- Configuration core -*- lexical-binding: t; -*-

(require 'core-packages (concat user-emacs-directory "core/core-packages.el"))
(require 'core-keybindings (concat user-emacs-directory "core/core-keybindings.el"))
(require 'core-emacs (concat user-emacs-directory "core/core-emacs.el"))
(require 'core-evil (concat user-emacs-directory "core/core-evil.el"))
(require 'core-completion (concat user-emacs-directory "core/core-completion.el"))

(provide 'core)
;;; core.el ends here
