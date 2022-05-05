;;; tools/bibliography.el -*- lexical-binding: t; -*-

(defconst my/bibliography-bibtex
  (file-truename "~/org/slip-box/literature/master.bib"))
(defconst my/bibliography-doc-directory
  (file-truename "~/org/slip-box/literature/docs"))
(defconst my/bibliography-notes
  (file-truename "~/org/slip-box/literature"))

(use-package ivy-bibtex
  :disabled
  :after ivy
  :commands (ivy-bibtex my/org-cite-open)
  :init
  ;; Define cite: org-link to be opened with bibtex-completion
  (with-eval-after-load 'org
    (defun my/org-cite-open (link)
      (let ((keys (list link)))
        (funcall
         (defhydra my/org-cite (:color teal :columns 2)
           "Cite"
           ("p" (bibtex-completion-open-pdf keys) "open pdf file (if present)")
           ("u" (bibtex-completion-open-url-or-doi keys) "open url or doi in browser")
           ("c" (bibtex-completion-insert-citation keys) "insert citation")
           ("r" (bibtex-completion-insert-reference keys) "insert reference")
           ("k" (bibtex-completion-insert-key keys) "insert bibtex key")
           ("b" (bibtex-completion-insert-bibtex keys) "insert bibtex entry")
           ("a" (bibtex-completion-add-PDF-attachment keys) "attach pdf to email")
           ("e" (bibtex-completion-edit-notes keys) "edit notes")
           ("s" (bibtex-completion-show-entry keys) "show entry")
           ("l" (bibtex-completion-add-pdf-to-library keys) "Add PDF to library")
           ("x" (my/bibtex-completion-open-pdf-external keys) "Open PDF file in external viewer (if present)")))))
    (org-link-set-parameters "cite"
                             :follow #'my/org-cite-open))
  :config
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-ignore-order))
  (setq bibtex-completion-find-additional-pdfs t
        bibtex-completion-bibliography my/bibliography-bibtex
        bibtex-completion-library-path my/bibliography-doc-directory
        bibtex-completion-notes-path my/bibliography-notes 
        ;; notes template
        bibtex-completion-notes-template-multiple-files
        ":PROPERTIES:
:ROAM_REFS: cite:${=key=}
:END:
#+TITLE: ${author-or-editor} - ${title}
#+CREATED:  %U

* Notes
:PROPERTIES:
:NOTER_DOCUMENT: ./docs/${=key=}.pdf
:END:
"
        ;; do not prompt for pre- and post-notes for LaTeX citations
        bibtex-completion-cite-prompt-for-optional-arguments nil
        ;; allow djvu files
        bibtex-completion-pdf-extension '(".pdf" ".djvu"))
  ;; FIXME: rewrite with org-roam-capture-
  (defun my/org-roam-add-id ()
    "Add ID to an org roam node."
    (interactive)
    (org-id-get-create)
    (org-roam-update-org-id-locations)
    (org-roam-db-sync))
  ;; Add action for opening PDFs externally.
  (defun my/bibtex-completion-open-pdf-external (keys &optional fallback-action)
    (let ((bibtex-completion-pdf-open-function
           (lambda (fpath) (call-process "xdg-open" nil 0 nil fpath))))
      (bibtex-completion-open-pdf keys fallback-action)))
  (ivy-bibtex-ivify-action my/bibtex-completion-open-pdf-external my/ivy-bibtex-open-pdf-external)
  (ivy-add-actions
   'ivy-bibtex
   '(("x" my/ivy-bibtex-open-pdf-external "Open PDF file in external viewer (if present)")))
  ;; Insert citation with "cite:" prefix in org mode.
  (defun my/bibtex-completion-format-citation-org-cite (keys)
    "Format org cite references for keys in KEYS."
    (s-join ", " (--map (format "cite:%s" it) keys)))
  (setf (alist-get 'org-mode bibtex-completion-format-citation-functions)
        #'my/bibtex-completion-format-citation-org-cite))

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

;; arxiv links.
(with-eval-after-load 'org
  (add-to-list 'org-link-abbrev-alist
               '("arxiv" . "https://arxiv.org/abs/")))

(use-package citar
  :config
  (setq citar-bibliography `(,my/bibliography-bibtex)
        citar-file-additional-files-separator "_"
        citar-file-note-extensions '("org")
        citar-library-paths `(,my/bibliography-doc-directory)
        citar-notes-paths `(,my/bibliography-notes)))
