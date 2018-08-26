;;; core-keybindings.el --- Keybindings system setup -*- lexical-binding: t; -*-

(defconst leader-key "SPC"
  "The leader prefix key for global commands.")

(defconst leader-key-non-normal "M-m"
  "The leader prefix key for global commands in emacs and insert states.")

(defconst local-leader-key "SPC m"
  "The local leader prefix key for major mode specific commands.")

(defconst local-leader-key-non-normal "M-,"
  "The local leader prefix key for major mode specific commands in emacs and insert states.")

(use-package general
  :demand
  :config
  (general-create-definer leader-def
    :states '(normal insert motion visual emacs)
    :keymaps 'override
    :prefix leader-key
    :non-normal-prefix leader-key-non-normal)
  (leader-def
    "" '(nil :which-key "leader prefix"))
  (general-create-definer local-leader-def
    :states '(normal insert motion visual emacs)
    :keymaps 'override
    :prefix local-leader-key
    :non-normal-prefix local-leader-key-non-normal)
  (local-leader-def
    "" '(nil :which-key "local-leader prefix")))

(use-package which-key
  :demand
  :config
  (setq which-key-sort-order #'which-key-description-order)
  (which-key-mode))

(use-package hydra
  :commands defhydra)

(provide 'core-keybindings)
;;; core-keybindings.el ends here
