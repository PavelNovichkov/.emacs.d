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
  :custom
  (pdf-cache-image-limit 1)
  (pdf-view-midnight-colors
   (cons nano-dark-foreground nano-dark-background))
  (pdf-view-midnight-invert nil)
  (pdf-view-continuous nil)
  (pdf-view-resize-factor 1.1)
  :config
  (pdf-tools-install))
