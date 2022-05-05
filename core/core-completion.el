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
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple))

(use-package embark-consult
  :demand :after embark)

;;; Avy

(use-package avy)

;;; Company

(use-package company
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

;;; Yasnippet

(use-package yasnippet)
