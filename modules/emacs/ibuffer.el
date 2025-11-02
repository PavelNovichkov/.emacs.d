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

(use-package all-the-icons-ibuffer
  :after ibuffer
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode)
  :config
  (setq all-the-icons-ibuffer-formats
        '((mark modified read-only locked " "
                (icon 2 2 :left :elide)
                #(" " 0 1
                  (display
                   (space :align-to 8)))
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " project-file-relative)
          (mark " "
                (name 16 -1)
                " " filename))))
