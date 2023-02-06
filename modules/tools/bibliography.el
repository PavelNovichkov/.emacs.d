;;; tools/bibliography.el -*- lexical-binding: t; -*-

(defconst my/bibliography-notes-subdir "literature"
  "Bibliography notes subdirectory in slip-box.")
(defconst my/bibliography-notes
  (expand-file-name my/bibliography-notes-subdir my/slip-box-directory)
  "Bibliography notes directory.")
(defconst my/bibliography-bibtex
  (expand-file-name "master.bib" my/bibliography-notes)
  "Bibliography BibTeX records file.")
(defconst my/bibliography-doc-directory
  (expand-file-name "docs" my/bibliography-notes)
  "Bibliography documents directory.")

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
  :init
  (setq org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar
        org-cite-global-bibliography (list my/bibliography-bibtex)
  :config
  (setq citar-bibliography (list my/bibliography-bibtex)
        citar-file-additional-files-separator "_"
        citar-file-note-extensions '("org")
        citar-library-paths (list my/bibliography-doc-directory)
        citar-notes-paths (list my/bibliography-notes))
  ;; Icons.
  (setq citar-symbols
        `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
          (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
          (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
  (setq citar-symbol-separator " ")
  (general-define-key
   :keymaps 'citar-map
   "a" #'citar-add-file-to-library))

(use-package citar-embark
  :straight nil ;; part of embark
  :demand :after citar
  :config
  (citar-embark-mode))

(use-package citar-org-roam
  :demand :after citar
  :custom
  (citar-org-roam-subdir my/bibliography-notes-subdir)
  (citar-org-roam-note-title-template "${author} - ${title}")
  :config
  (citar-org-roam-mode))
