;;; lang/julia.el -*- lexical-binding: t; -*-

(use-package julia-mode)

(use-package eglot-jl
  :demand :after julia-mode
  :config
  (eglot-jl-init))
