;;; emacs/helpful.el -*- lexical-binding: t; -*-

(use-package helpful
  :commands (helpful-at-point
             helpful-callable
             helpful-key
             helpful-symbol
             helpful-variable)
  :config
  ;; (setq counsel-describe-function-function #'helpful-callable
  ;;       counsel-describe-variable-function #'helpful-variable)
  )
