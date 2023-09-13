;;; app/terminal.el -*- lexical-binding: t; -*-

(use-package vterm
  :ensure-system-package
  (("/usr/lib64/libvterm.so" . libvterm)
   (cmake . cmake))
  :commands vterm
  :custom
  (vterm-buffer-name-string "vterm %s")
  (vterm-shell "/usr/bin/fish")
  :config
  (add-to-list
   'display-buffer-alist
   '("vterm.*"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 0)
     (window-height . 10))))
