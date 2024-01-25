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
  (straight-pull-all)
  (straight-rebuild-all)
  (straight-freeze-versions))

(defun my/revert-packages ()
  "Revert packages to previous version."
  (interactive)
  (let ((lockfile (straight--versions-file "default.el"))
        (backup-lockfile (straight--versions-file "backup.el")))
    (copy-file backup-lockfile lockfile t))
  (straight-thaw-versions)
  (straight-rebuild-all))

;; Install org early before the built-in version gets loaded. Use the
;; latest stable version by adding :branch "bugfix" to the standard
;; recipe.
(let* ((old-plist (cdr (straight-recipes-retrieve 'org)))
       (new-plist (plist-put old-plist :branch "bugfix"))
       (recipe (cons 'org new-plist)))
  (straight-use-package recipe))

;;; use-package
(setq straight-use-package-by-default t)

(setq use-package-always-defer t)

(setq use-package-compute-statistics t)

(use-package use-package-ensure-system-package ; built-in
  :straight nil)

;;; Use consistent configuration paths.
(use-package no-littering
  :demand
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))
