;;; emacs/dired.el -*- lexical-binding: t; -*-

(use-package dired ; built-in
  :straight nil
  :init
  (defun my/dired-update-default-directory ()
    "Update `default-directory' to parent directory at point."
    (ignore-errors
      (setq-local default-directory
                  (file-name-directory (dired-get-file-for-visit)))))
  (defun my/add-hooks-in-dired-mode ()
    "Add hooks to run in dired mode."
    (add-hook 'post-command-hook #'my/dired-update-default-directory nil t))
  (add-hook 'dired-mode-hook #'my/add-hooks-in-dired-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)
  :config
  (setq dired-auto-revert-buffer t
        dired-hide-details-hide-symlink-targets nil
        dired-recursive-copies 'always
        dired-recursive-deletes 'top)
  ;; Hide details.
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
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
  (define-key dired-mode-map (kbd "s-o") #'my/dired-open-native)
  ;; Ediff marked files, taken from
  ;; https://oremacs.com/2017/03/18/dired-ediff/.
  (defun my/dired-ediff-files ()
    "Ediff files from dired."
    (interactive)
    (let ((files (dired-get-marked-files)))
      (if (<= (length files) 2)
          (let ((file1 (car files))
                (file2 (if (cdr files)
                           (cadr files)
                         (read-file-name
                          "File B to compare: "
                          (dired-dwim-target-directory)))))
            (ediff-files file1 file2))
        (error "No more than 2 files should be marked"))))
  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map
   "=" #'my/dired-ediff-files))

(use-package dired-filter
  :demand :after dired
  :init
  (setq dired-filter-stack nil))

(use-package dired-subtree
  :commands dired-subtree-toggle
  :init
  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map
   "TAB" #'dired-subtree-toggle)
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package diredfl
  :demand :after dired
  :config
  (diredfl-global-mode))
