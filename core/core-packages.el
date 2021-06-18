;;; core-packages.el --- Packaging system setup -*- lexical-binding: t; -*-

;;; straight
(setq straight-check-for-modifications nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defun my/update-packages ()
  "Update packages and lockfile."
  (interactive)
  (let ((lockfile (straight--versions-file "default.el"))
        (backup-lockfile (straight--versions-file "backup.el")))
    (copy-file lockfile backup-lockfile t))
  (straight-pull-all)
  (straight-rebuild-all)
  (straight-freeze-versions))

;;; use-package
(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(setq use-package-always-defer t)

(setq use-package-compute-statistics t)

(use-package use-package-ensure-system-package
  :ensure t)

;;; Use consistent configuration paths.
(use-package no-littering
  :demand
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(provide 'core-packages)
;;; core-packages.el ends here
