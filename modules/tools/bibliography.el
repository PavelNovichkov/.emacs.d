;;; tools/bibliography.el -*- lexical-binding: t; -*-

(defconst my/bibliography-notes-subdir "literature"
  "Bibliography notes subdirectory in slip-box.")
(defconst my/bibliography-notes
  (expand-file-name my/bibliography-notes-subdir my/slip-box-directory)
  "Bibliography notes directory.")
(defconst my/bibliography-bibtex
  (list
   (expand-file-name "master.bib" my/bibliography-notes)
   (file-truename "~/data/library/catalog.bib"))
  "Bibliography BibTeX records files.")
(defconst my/bibliography-doc-directory
  (expand-file-name "docs" my/bibliography-notes)
  "Bibliography documents directory.")

;; arxiv links.
(with-eval-after-load 'org
  (add-to-list 'org-link-abbrev-alist
               '("arxiv" . "https://arxiv.org/abs/")))

(use-package citar
  :init
  (setq org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar
        org-cite-global-bibliography my/bibliography-bibtex
        ; Export as \cite command rather than \autocite.
        org-cite-export-processors '((latex biblatex nil "nil/bare")))
  :config

  (setq citar-bibliography my/bibliography-bibtex
        citar-file-additional-files-separator "_"
        citar-file-note-extensions '("org")
        citar-library-paths (list my/bibliography-doc-directory)
        citar-notes-paths (list my/bibliography-notes))

  ;; Fixes an issue when <tab> is different from C-i, see
  ;; https://github.com/emacs-citar/citar/issues/802.
  (setq citar--multiple-setup '("<tab>" . "RET"))

  (add-to-list
   'citar-templates
   '(suffix . " [${=key= id}]"))
  (add-to-list
   'citar-templates
   '(main . "${author editor:%sn}, “${title}” (${date year issued:4})"))
  (setopt citar-indicators nil)
  
  (general-define-key
   :keymaps 'citar-map
   "a" #'citar-add-file-to-library))

(use-package citar-embark
  :demand :after citar
  :config
  (citar-embark-mode))

(use-package citar-org-roam
  :demand :after citar
  :custom
  (citar-org-roam-subdir my/bibliography-notes-subdir)
  (citar-org-roam-note-title-template "${author:%sn} - ${title}")
  :config
  (citar-org-roam-mode))
