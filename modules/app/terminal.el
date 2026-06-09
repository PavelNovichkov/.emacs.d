;;; app/terminal.el -*- lexical-binding: t; -*-

(use-package vterm
  :ensure-system-package
  ((cmake . cmake))

  :commands (vterm my/terminal my/project-terminal)

  :custom
  (vterm-buffer-name-string "*vterm %s*")
  (vterm-shell "/usr/bin/fish")

  :config
  (general-define-key
   :keymaps 'vterm-mode-map
   :states '(emacs insert normal)
   "C-q" #'vterm-send-next-key)

  (defun my/terminal ()
    "Start vterm in default directory, or switch to an existing session."
    (interactive)
    (let* ((terminal-buffer-name
            (format vterm-buffer-name-string (directory-file-name default-directory)))
           (terminal-buffer (get-buffer terminal-buffer-name)))
      (if terminal-buffer
          (pop-to-buffer terminal-buffer)
        (vterm))))

  (require 'project)
  (defun my/project-terminal ()
    "Start vterm in project's root directory, or switch to an existing session."
    (interactive)
    (let ((default-directory (file-truename (project-root (project-current t)))))
      (my/terminal)))

  (add-to-list
   'display-buffer-alist
   '("^\\*vterm"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 0)
     (window-height . 10))))
