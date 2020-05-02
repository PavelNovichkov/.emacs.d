;;; emacs/ediff.el -*- lexical-binding: t; -*-

(use-package ediff ; built-in
  :straight nil
  :init
  ;; Setup in org-mode.
  (defun my/setup-ediff-in-org-mode ()
    "Expand headings before running ediff."
    (add-hook 'ediff-prepare-buffer-hook #'org-show-all nil t))
  (add-hook 'org-mode-hook #'my/setup-ediff-in-org-mode)
  ;; Setup in outline-minor-mode.
  (defun my/setup-ediff-in-outline-minor-mode ()
    "Expand headings before running ediff."
    (add-hook 'ediff-prepare-buffer-hook #'outline-show-all nil t))
  (add-hook 'outline-minor-mode-hook #'my/setup-ediff-in-outline-minor-mode)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-highlight-all-diffs nil))
