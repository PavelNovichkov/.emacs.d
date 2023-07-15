;;; tools/pdf.el -*- lexical-binding: t; -*-

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :init
  ;; Get rid of the one-pixel border surrounding the PDF. See
  ;; https://www.reddit.com/r/emacs/comments/dgywoo/issue_with_pdfview_midnight_mode/.
  (defun my/pdf-tools-disable-evil-cursor ()
    (set (make-local-variable 'evil-normal-state-cursor)
         (list nil)))
  :hook
  (pdf-view-mode . pdf-sync-minor-mode)
  (pdf-view-mode . my/pdf-tools-disable-evil-cursor)
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
