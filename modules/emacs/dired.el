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
  (setq dired-auto-revert-buffer t
        dired-create-destination-dirs 'ask
        dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil
        ;; Human-readable sizes; directories first.
        dired-listing-switches "-alh --group-directories-first"
        dired-recursive-copies 'always
        dired-recursive-deletes 'top)
  ;; Hide details.
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  ;; Allow 'a' command in dired-mode.
  (put 'dired-find-alternate-file 'disabled nil))

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

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  ;; Fix icons in subtrees, see
  ;; https://github.com/jtbm37/all-the-icons-dired/issues/22.
  (advice-add #'dired-subtree-toggle :around #'all-the-icons-dired--refresh-advice))
