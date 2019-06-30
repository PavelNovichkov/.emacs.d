;;; tools/pdf.el -*- lexical-binding: t; -*-

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . pdf-sync-minor-mode)
  :config
  (pdf-tools-install)
  (setq pdf-cache-image-limit 1
        pdf-view-continuous nil
        pdf-view-resize-factor 1.1))

;; Add support for org links.
(use-package org-pdfview
  :after org
  :commands (org-pdfview-open
             org-pdfview-store-link)
  :init
  ;; Make pdfview hyperlink known to org before the package is loaded.
  (org-link-set-parameters "pdfview"
                           :follow #'org-pdfview-open
                           :complete #'org-pdfview-complete-link
                           :store #'org-pdfview-store-link))
