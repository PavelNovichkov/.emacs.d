;;; tools/bibliography.el -*- lexical-binding: t; -*-

(defconst my/bibliography-bibtex
  (file-truename "~/org/slip-box/literature/master.bib"))
(defconst my/bibliography-doc-directory
  (file-truename "~/org/slip-box/literature/docs"))
(defconst my/bibliography-notes
  (file-truename "~/org/slip-box/literature"))
(defconst my/org-roam-note-header
  ":PROPERTIES:
:CREATED:  %U
:ROAM_REFS: [cite:@${citekey}]
:END:
#+TITLE: ${title}\n")

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
        org-cite-global-bibliography `(,my/bibliography-bibtex))
  :config
  (setq citar-bibliography `(,my/bibliography-bibtex)
        citar-file-additional-files-separator "_"
        citar-file-note-extensions '("org")
        citar-library-paths `(,my/bibliography-doc-directory)
        citar-notes-paths `(,my/bibliography-notes))
  ;; Icons.
  (setq citar-symbols
        `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
          (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
          (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
  (setq citar-symbol-separator " ")
  ;; Refresh the bibliography.
  (citar-filenotify-setup '(LaTeX-mode-hook org-mode-hook))
  (setq citar-filenotify-callback 'refresh-cache)
  ;; Create notes.
  (defun my/citar-org-format-note (key entry filepath)
    "Format a note FILEPATH from KEY and ENTRY."
    (let ((title (citar--format-entry-no-widths entry "${author} - ${title}")))
      (org-roam-capture- :templates
                         `(("r" "reference" plain "%?" :if-new
                            (file+head ,filepath ,my/org-roam-note-header)
                            :immediate-finish t
                            :unnarrowed t))
                         :info (list :citekey key)
                         :node (org-roam-node-create :title title)
                         :props '(:finalize find-file))))
  (setq citar-format-note-function #'my/citar-org-format-note))
