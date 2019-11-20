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
 ;; no bells
 ring-bell-function #'ignore
 visible-bell nil
 ;; scrolling
 hscroll-step 1
 scroll-conservatively 101
 scroll-margin 0
 scroll-preserve-screen-position t
 ;; editor
 indent-tabs-mode nil
 require-final-newline t
 save-interprogram-paste-before-kill t
 sentence-end-double-space nil
 word-wrap t
 )

;; Wrap lines in all text buffers.
(add-hook 'text-mode-hook #'visual-line-mode)

;; Replace yes/no with y/n.
(fset #'yes-or-no-p #'y-or-n-p)

(use-package server ; built-in
  :if (display-graphic-p)
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

(use-package undo-tree
  :demand
  :config
  (setq undo-tree-auto-save-history t)
  ;; Undo in region is buggy, so disable it.
  (setq undo-tree-enable-undo-in-region nil)
  (global-undo-tree-mode))

;; Workaround fixing Emacs crash on GTK when showing Noto Color Emoji
;; fonts. References:
;; - https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=893319
;; - https://askubuntu.com/questions/1076735/emacs-crashes-on-pasting-the-unicode-symbol
(when window-system
  (push "Noto Color Emoji" face-ignored-fonts))

(provide 'core-emacs)
;;; core-emacs.el ends here
