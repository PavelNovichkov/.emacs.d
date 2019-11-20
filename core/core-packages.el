;;; core-packages.el --- Packaging system setup -*- lexical-binding: t; -*-

;;; package.el
(setq package-enable-at-startup nil
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-defer t
      use-package-always-ensure t)

(setq use-package-compute-statistics t)

;;; Use consistent configuration paths.
(use-package no-littering
  :demand
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(provide 'core-packages)
;;; core-packages.el ends here
