;;; tools/bibliography.el -*- lexical-binding: t; -*-

(defconst my/bibliography-bibtex
  (expand-file-name "~/org/reference/papers/master.bib"))
(defconst my/bibliography-pdf-directory
  (expand-file-name "~/org/reference/papers/pdfs/"))
(defconst my/bibliography-notes
  (expand-file-name "~/org/reference/papers/notes.org"))

(use-package ivy-bibtex
  :after ivy
  :commands ivy-bibtex
  :config
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-ignore-order))
  (setq bibtex-completion-bibliography my/bibliography-bibtex
        bibtex-completion-library-path my/bibliography-pdf-directory
        bibtex-completion-notes-path my/bibliography-notes 
        ;; notes template
        bibtex-completion-notes-template-one-file "
* ${author-or-editor} (${year}): ${title}
:PROPERTIES:
:Custom_ID: ${=key=}
:INTERLEAVE_PDF: ./pdfs/${=key=}.pdf
:END:
"
        ;; do not prompt for pre- and post-notes for LaTeX citations
        bibtex-completion-cite-prompt-for-optional-arguments nil)
  ;; Add action for opening PDFs externally.
  (defun my/bibtex-completion-open-pdf-external (keys &optional fallback-action)
    (let ((bibtex-completion-pdf-open-function
           (lambda (fpath) (call-process "xdg-open" nil 0 nil fpath))))
      (bibtex-completion-open-pdf keys fallback-action)))
  (ivy-bibtex-ivify-action my/bibtex-completion-open-pdf-external my/ivy-bibtex-open-pdf-external)
  (ivy-add-actions
   'ivy-bibtex
   '(("x" my/ivy-bibtex-open-pdf-external "Open PDF file in external viewer (if present)"))))

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
