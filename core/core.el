;;; core.el --- Configuration core -*- lexical-binding: t; -*-

(require 'core-packages (concat user-emacs-directory "core/core-packages.el"))
(require 'core-keybindings (concat user-emacs-directory "core/core-keybindings.el"))
(require 'core-emacs (concat user-emacs-directory "core/core-emacs.el"))

(provide 'core)
;;; core.el ends here
