;;; evil/plugins.el -*- lexical-binding: t; -*-

(use-package evil-commentary
  :commands (evil-commentary
             evil-commentary-yank)
  :init
  (general-define-key
   :states 'normal
   "gc" #'evil-commentary
   "gy" #'evil-commentary-yank))

(use-package evil-exchange
  :commands evil-exchange
  :init
  (general-define-key
   :states '(normal visual)
   "zx" #'evil-exchange)
  :config
  (setopt
   evil-exchange-highlight-face 'evil-ex-lazy-highlight)
  ;; Cancel exchange on escape.
  (defun my/escape-evil-exchange ()
    "Cancel evil-exchange."
    (when evil-exchange--overlays
      (evil-exchange-cancel)
      t))
  (add-hook 'my/escape-hook #'my/escape-evil-exchange))

(use-package evil-numbers
  :init
  (general-define-key
   :states '(normal visual)
   "g=" #'evil-numbers/inc-at-pt
   "g-" #'evil-numbers/dec-at-pt)
  (general-define-key
   :states 'visual
   "g+" #'evil-numbers/inc-at-pt-incremental
   "g_" #'evil-numbers/dec-at-pt-incremental))

(use-package evil-owl
  :demand :after evil
  :config
  (setq evil-owl-display-method 'posframe
        evil-owl-extra-posframe-args
        (list
         :width 50
         :height 20
         :internal-border-width 1
         :internal-border-color "black")
        evil-owl-max-string-length 50)
  (evil-owl-mode))

(use-package evil-surround
  :commands (evil-surround-edit
             evil-Surround-edit
             evil-surround-region
             evil-Surround-region)
  :init
  (general-define-key
   :states '(normal operator)
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
