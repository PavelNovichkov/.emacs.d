;;; core-keybindings.el --- Keybindings system setup -*- lexical-binding: t; -*-

(defconst leader-key "SPC"
  "The leader prefix key for global commands.")

(defconst leader-key-non-normal "C-SPC"
  "The leader prefix key for global commands in emacs and insert states.")

(defconst local-leader-key "SPC m"
  "The local leader prefix key for major mode specific commands.")

(defconst local-leader-key-non-normal "C-SPC m"
  "The local leader prefix key for major mode specific commands in emacs and insert states.")

(use-package general
  :ensure (:wait t)
  :demand
  :config
  (general-create-definer leader-def
    :states '(normal insert motion visual emacs)
    :keymaps 'override
    :prefix leader-key
    :non-normal-prefix leader-key-non-normal)
  (leader-def
    "" '("leader" . nil))
  (general-create-definer local-leader-def
    :states '(normal insert motion visual emacs)
    :keymaps 'override
    :prefix local-leader-key
    :non-normal-prefix local-leader-key-non-normal)
  (local-leader-def
    "" '("mode" . nil)))

(use-package which-key ; built-in
  :ensure nil
  :demand
  :custom
  (which-key-sort-order #'which-key-description-order)
  ;; needed when frame has internal border
  (which-key-allow-imprecise-window-fit nil)
  (which-key-mode t))

(use-package hydra
  :commands defhydra)

;;; Universal escape (stolen from Doom Emacs).

(defvar my/escape-hook nil
  "A hook run when C-g or ESC is pressed.")

(defun my/escape ()
  "Universal escape function."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'my/escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))

(general-define-key [remap keyboard-quit] #'my/escape)

;;; Distinguish <tab> from TAB = C-i.
(general-define-key
 :keymaps 'input-decode-map
 "C-i" [C-i])
