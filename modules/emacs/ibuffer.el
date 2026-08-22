;;; emacs/ibuffer.el -*- lexical-binding: t; -*-

(use-package ibuffer ; built-in
  :ensure nil
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :config
  (setq ibuffer-show-empty-filter-groups nil))

(use-package ibuffer-project
  :init
  (defun my/ibuffer-project-set-filter-groups ()
    "Set ibuffer groups based on project root of buffers."
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative)))
  :hook (ibuffer . my/ibuffer-project-set-filter-groups))
