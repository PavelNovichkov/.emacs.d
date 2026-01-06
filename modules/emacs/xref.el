;;; emacs/xref.el -*- lexical-binding: t; -*-

(use-package xref ; built-in
  :ensure nil
  :config
  (setopt
   xref-search-program 'ripgrep
   xref-show-definitions-function #'consult-xref
   xref-show-xrefs-function #'consult-xref))
