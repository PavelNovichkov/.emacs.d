;;; emacs/dired.el -*- lexical-binding: t; -*-

(use-package dired ; built-in
  :ensure nil
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
  ;; Override evil-collection binding.
  (defun my/dired-override-bindings (mode &rest _rest)
    "Rebind '=' set by both dired and evil-collection."
    (when (equal mode 'dired)
      (general-define-key
       :states 'normal
       :keymaps 'dired-mode-map
       "=" #'my/dired-ediff-files)
      (remove-hook 'evil-collection-setup-hook #'my/dired-override-bindings)))
  (add-hook 'evil-collection-setup-hook #'my/dired-override-bindings)
  :config
  (setopt
   dired-auto-revert-buffer #'dired-directory-changed-p
   dired-create-destination-dirs 'ask
   dired-create-destination-dirs-on-trailing-dirsep t
   dired-do-revert-buffer t
   dired-dwim-target t
   dired-hide-details-hide-symlink-targets nil
   dired-kill-when-opening-new-dired-buffer t
   ;; Human-readable sizes; directories first.
   dired-listing-switches "-alh --group-directories-first"
   dired-recursive-copies 'always
   dired-recursive-deletes 'always
   dired-vc-rename-file t)
  ;; Hide details.
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

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

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))
