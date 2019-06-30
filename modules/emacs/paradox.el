;;; emacs/paradox.el -*- lexical-binding: t; -*-

(use-package paradox
  :commands (paradox-list-packages
             paradox-upgrade-packages)
  :config
  (setq paradox-lines-per-entry 1
        paradox-column-width-package 25
        paradox-column-width-version 13
        paradox-execute-asynchronously t))
