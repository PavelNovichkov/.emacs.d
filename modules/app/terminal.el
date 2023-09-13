;;; app/terminal.el -*- lexical-binding: t; -*-

(use-package vterm
  :ensure-system-package
  (("/usr/lib64/libvterm.so" . libvterm)
   (cmake . cmake))
  :commands vterm
  :custom
  (vterm-buffer-name-string "vterm %s")
  (vterm-shell "/usr/bin/fish"))
