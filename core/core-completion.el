;;; core-completion.el --- Completion settings -*- lexical-binding: t; -*-

;;; Vertico (completion UI)

(use-package vertico
  :straight (:host github
             :repo "emacs-straight/vertico"
             :files ("*.el" "extensions/*.el"))
  :demand
  :custom
  (enable-recursive-minibuffers t)
  (vertico-cycle t)
  (vertico-mode t))

(use-package vertico-quick
  :straight nil ;; part of vertico
  :demand :after vertico
  :custom
  (vertico-quick1 "tnseriaodh")
  (vertico-quick2 "")
  :config
  (general-define-key
   :keymaps 'vertico-map
   :states 'insert
   "C-s" #'vertico-quick-exit))

(use-package vertico-repeat
  :straight nil ;; part of vertico
  :demand :after vertico
  :hook (minibuffer-setup . vertico-repeat-save))

;;; Orderless (completion matching style)

(use-package orderless
  :demand
  :custom
  (completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion))))
  (orderless-matching-styles
   '(orderless-literal orderless-regexp orderless-initialism)))

;;; Marginalia (completion annotations)

(use-package marginalia
  :demand
  :custom
  (marginalia-mode t))

;;; Embark (completion actions and more)

(use-package embark
  :straight (:host github
             :repo "oantolin/embark"
             :files ("*.el"))
  :init
  (general-define-key
   :states '(insert motion normal emacs)
   :keymaps 'override
   "C-." #'embark-act
   "M-." #'embark-dwim)
  (general-define-key
   :states '(insert normal emacs)
   :keymaps 'vertico-map
   "C-b" #'embark-become
   "C-e" #'embark-export)
  :custom
  ;; Press "?" after a prefix to show keybindings in the minibuffer.
  (embark-help-key "?")
  (prefix-help-command #'embark-prefix-help-command))

;;; Consult (completion-based commands)

(use-package consult
  :demand
  :custom
  (consult-narrow-key "<")
  (consult-preview-key nil))

(use-package embark-consult
  :straight nil ;; part or embark
  :demand :after embark
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; Avy

(use-package avy
  :custom
  ;; Use Colemak-friendly set of keys.
  (avy-keys '(?t ?s ?r ?a ?n ?e ?i ?o)))

;;; Corfu

(use-package corfu
  :straight (:host github
             :repo "minad/corfu"
             :files ("*.el" "extensions/*.el"))
  :demand
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (global-corfu-mode t)
  (evil-collection-corfu-key-themes nil)
  :config
  (defun my/corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer."
    (when (local-variable-p 'completion-at-point-functions)
      (setq-local
       ;; Disable automatic echo and popup
       corfu-echo-delay nil
       corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'my/corfu-enable-in-minibuffer)
  (general-define-key
   :keymaps 'corfu-map
   :states 'insert
   "<escape>" #'corfu-quit
   "C-SPC" #'corfu-insert-separator))

(use-package corfu-quick
  :straight nil ;; part of corfu
  :demand :after corfu
  :custom
  (corfu-quick1 "tnseriaodh")
  (corfu-quick2 "")
  :config
  (general-define-key
   :keymaps 'corfu-map
   :states 'insert
   "C-s" #'corfu-quick-insert))

(use-package cape
  :demand :after corfu
  :config
  ; TODO How to set these by default?
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package kind-icon
  :demand :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-default-style
   '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.4 :scale 1.0))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
