;;; tools/bibliography.el -*- lexical-binding: t; -*-

(defconst my/bibliography-bibtex
  (expand-file-name "~/org/slip-box/literature/master.bib"))
(defconst my/bibliography-doc-directory
  (expand-file-name "~/org/slip-box/literature/docs"))
(defconst my/bibliography-notes
  (expand-file-name "~/org/slip-box/literature"))

(use-package ivy-bibtex
  :after ivy
  :commands ivy-bibtex
  :config
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-ignore-order))
  (setq bibtex-completion-bibliography my/bibliography-bibtex
        bibtex-completion-library-path my/bibliography-doc-directory
        bibtex-completion-notes-path my/bibliography-notes 
        ;; notes template
        bibtex-completion-notes-template-multiple-files
        "#+TITLE: ${author-or-editor} - ${title}
#+ROAM_KEY: cite:${=key=}
#+CREATED:  %U

* Notes
:PROPERTIES:
:NOTER_DOCUMENT: ./docs/${=key=}.pdf
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
  (setq org-noter-always-create-frame nil
        org-noter-default-notes-file-names nil
        org-noter-doc-split-fraction '(0.7 . 0.5)
        org-noter-hide-other t
        org-noter-insert-note-no-questions t))
