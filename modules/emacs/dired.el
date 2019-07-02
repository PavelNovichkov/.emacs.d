;;; emacs/dired.el -*- lexical-binding: t; -*-

(use-package dired ; built-in
  :ensure nil
  :config
  ;; Allow 'a' command in dired-mode.
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-dwim-target t)
  ;; Human-readable sizes; directories first.
  (setq dired-listing-switches "-alh --group-directories-first")
  ;; Native open.
  (defun my/dired-open-native ()
    "Open marked files (or the file the cursor is on) from dired."
    (interactive)
    (let* ((files (dired-get-marked-files t current-prefix-arg))
           (n (length files)))
      (when (or (<= n 3)
                (y-or-n-p (format "Open %d files?" n)))
        (dolist (file files)
          (call-process "xdg-open"
                        nil 0 nil file)))))
  (define-key dired-mode-map (kbd "s-o") #'my/dired-open-native))

(use-package dired-subtree
  :commands dired-subtree-toggle
  :init
  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map
   "TAB" #'dired-subtree-toggle)
  :config
  (setq dired-subtree-use-backgrounds nil))
