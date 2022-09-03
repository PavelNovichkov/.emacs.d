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

(use-package vertico-repeat
  :straight nil ;; part of vertico
  :demand :after vertico
  :hook (minibuffer-setup . vertico-repeat-save))

;;; Orderless (completion matching style)

(use-package orderless
  :demand
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism)))

;;; Marginalia (completion annotations)

(use-package marginalia
  :demand
  :custom
  (marginalia-mode t))

;;; Embark (completion actions and more)

(use-package embark
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
  :config
  ;; Press "?" after a prefix to show keybindings in the minibuffer.
  (setq embark-help-key "?"
        prefix-help-command #'embark-prefix-help-command))

;;; Consult (completion-based commands)

(use-package consult
  :demand
  :config
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (setq consult-narrow-key "<"))

(use-package embark-consult
  :demand :after embark)

;;; Avy

(use-package avy
  :config
  ;; Use Colemak-friendly set of keys.
  (setq avy-keys '(?t ?s ?r ?a ?n ?e ?i ?o)))

;;; Company

(use-package company
  :disabled
  :demand
  :commands company-complete-common
  :config
  ;; (company-tng-mode +1) is set up by evil-collection.
  (setq company-backends '(company-files company-capf)
        company-idle-delay 0.25
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-tooltip-width-grow-only t)
  (global-company-mode 1)
  (general-define-key
   :keymaps 'company-active-map
   "C-/" #'company-filter-candidates)
  (general-define-key
   :keymaps 'company-search-map
   "C-n" #'company-select-next-or-abort
   "C-p" #'company-select-previous-or-abort)
  (dotimes (i 10)
    (general-define-key
     :keymaps '(company-active-map company-search-map)
     (format "C-%d" i) #'company-complete-number)))

;;; Corfu

(use-package corfu
  :straight (:host github
             :repo "minad/corfu"
             :files ("*.el" "extensions/*.el"))
  :demand
  :config
  (setq corfu-auto t
        corfu-cycle t)
  (global-corfu-mode)
  (defun my/corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'my/corfu-enable-in-minibuffer)
  (defun my/corfu-move-to-minibuffer ()
    "Export Corfu completions to the minibuffer for more advanced processing."
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (general-define-key
   :keymaps 'corfu-map
   :states 'insert
   "C-SPC" #'corfu-insert-separator
   "C-e" #'my/corfu-move-to-minibuffer))

(use-package corfu-quick
  :straight nil ;; part of corfu
  :demand :after corfu
  :config
  (general-define-key
   :keymaps 'corfu-map
   :states 'insert
   "C-j" #'corfu-quick-insert))

(use-package cape
  :demand :after corfu
  :config
  ; TODO How to set these by default?
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package kind-icon
  :demand :after corfu
  :config
  (setq kind-icon-default-face 'corfu-default)
  (setq kind-icon-default-style
        '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 1.0))
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; Yasnippet

(use-package yasnippet)
