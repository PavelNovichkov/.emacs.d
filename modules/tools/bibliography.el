;;; tools/bibliography.el -*- lexical-binding: t; -*-

;; FIXME: does not load.
(use-package org-ref
  :after org
  :init
  (setq org-ref-bibliography-notes (expand-file-name "~/org/reference/papers/notes.org")
        org-ref-default-bibliography `(,(expand-file-name "~/org/reference/papers/master.bib"))
        org-ref-pdf-directory (expand-file-name "~/org/reference/papers/pdfs/"))
  (setq bibtex-completion-bibliography org-ref-default-bibliography
        bibtex-completion-library-path org-ref-pdf-directory
        bibtex-completion-notes-path org-ref-bibliography-notes))

(use-package org-noter
  :after org
  :commands org-noter
  :config
  ;; Don't add notes count to the modeline.
  (advice-add #'org-noter--mode-line-text :override #'ignore)
  (setq org-noter-property-doc-file "INTERLEAVE_PDF"
        org-noter-property-note-location "INTERLEAVE_PAGE_NOTE"
        org-noter-always-create-frame nil
        org-noter-default-notes-file-names `(,org-ref-bibliography-notes)
        org-noter-doc-split-fraction '(0.7 . 0.5)
        org-noter-hide-other t
        org-noter-insert-note-no-questions t))
