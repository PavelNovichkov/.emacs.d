;;; app/terminal.el -*- lexical-binding: t; -*-

(use-package vterm
  :ensure-system-package
  ((vterm-ctrl . libvterm)
   (cmake . cmake))
  :commands vterm)
