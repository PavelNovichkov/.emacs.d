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
(use-package org-pdftools
  :after org
  :commands (org-pdftools-open
             org-pdftools-store-link)
  :init
  ;; Make pdf hyperlink known to org before the package is loaded.
  (org-link-set-parameters "pdf"
                           :follow #'org-pdftools-open
                           :complete #'org-pdftools-complete-link
                           :store #'org-pdftools-store-link
                           :export #'org-pdftools-export))
