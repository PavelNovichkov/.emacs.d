;;; core-emacs.el --- Emacs settings -*- lexical-binding: t; -*-

;; Enhance security.
;; Reference: https://glyph.twistedmatrix.com/2015/11/editor-malware.html.
(setq tls-checktrust t
      gnutls-verify-error t
      tls-program '("gnutls-cli --x509cafile %t -p %p %h"))

;; safe-local-variable-values are saved to custom-file, so we have to load it.
(load custom-file t t)

(setq-default
 ;; quiet startup
 inhibit-startup-message t
 initial-scratch-message nil
 ;; frame title
 frame-title-format "Emacs"
 ;; no bells
 ring-bell-function #'ignore
 visible-bell nil
 ;; no warnings from native compilation
 native-comp-async-report-warnings-errors 'silent
 ;; no dialog boxes
 use-dialog-box nil
 ;; editor
 indent-tabs-mode nil
 require-final-newline t
 save-interprogram-paste-before-kill t
 sentence-end-double-space nil
 word-wrap t
 ;; use system trash
 delete-by-moving-to-trash t
 ;; disable bidirectional editing
 bidi-inhibit-bpa t
 bidi-paragraph-direction 'left-to-right
 ;; no backups and lockfiles
 make-backup-files nil
 create-lockfiles nil
 ;; highlight current error/occur/grep item etc.
 next-error-message-highlight t
 )

;; Auto-save.
(setq auto-save-default t
      auto-save-include-big-deletions t
      ;; Store auto-save files in the var directory.
      auto-save-file-name-transforms
      `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
         ;; Prefix tramp autosave to prevent conflicts.
         ,(no-littering-expand-var-file-name "auto-save/tramp-\\2") t)
        (".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Wrap lines in all text buffers.
(add-hook 'text-mode-hook #'visual-line-mode)

(use-package adaptive-wrap
  :custom
  (adaptive-wrap-extra-indent 2)
  :hook
  (prog-mode . adaptive-wrap-prefix-mode))

;; Replace yes/no with y/n.
(setq use-short-answers t)

(use-package server ; built-in
  :straight nil
  :demand
  :custom
  ;; Open files in a new tab.
  (server-window #'switch-to-buffer-other-tab))

;; Save minibuffer history across sessions.
(use-package savehist ; built-in
  :straight nil
  :demand
  :custom
  (history-length 10)
  (savehist-autosave-interval nil)
  (savehist-mode t))

(use-package undo-fu
  :config
  (general-define-key [remap undo] #'undo-fu-only-undo))

(use-package sudo-edit
  :init
  (general-define-key
   :keymaps 'embark-file-map
   "s" #'sudo-edit-find-file))

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t))
