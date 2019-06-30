;;; evil/plugins.el -*- lexical-binding: t; -*-

(use-package evil-commentary
  :commands (evil-commentary
             evil-commentary-yank)
  :init
  (general-define-key
   :states 'normal
   "gc" #'evil-commentary
   "gy" #'evil-commentary-yank))

(use-package evil-escape
  :demand :after evil
  :config
  (setq-default evil-escape-key-sequence "fj"
                evil-escape-unordered-key-sequence t)
  (evil-escape-mode))

(use-package evil-surround
  :commands (evil-surround-edit
             evil-Surround-edit
             evil-surround-region
             evil-surround-region)
  :init
  (general-define-key
   :states 'operator
   "s" #'evil-surround-edit
   "S" #'evil-Surround-edit)
  (general-define-key
   :states 'visual
   "s" #'evil-surround-region
   "S" #'evil-Surround-region))

(use-package evil-visualstar
  :commands (evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (general-define-key
   :states 'visual
   "*" #'evil-visualstar/begin-search-forward
   "#" #'evil-visualstar/begin-search-backward))
