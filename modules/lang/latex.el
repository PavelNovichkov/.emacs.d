;;; lang/latex.el -*- lexical-binding: t; -*-

(use-package tex
  :straight auctex
  :hook ((TeX-mode . outline-minor-mode)
         (TeX-mode . prettify-symbols-mode)
         (TeX-mode . TeX-source-correlate-mode))
  :config
  (setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "PDF Tools")
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (setq TeX-parse-self t
        TeX-auto-save t
        TeX-auto-local ".auctex-auto"
        TeX-style-local ".auctex-style"
        TeX-error-overview-open-after-TeX-run t)
  (setq font-latex-fontify-script nil
        font-latex-fontify-sectioning 'color)
  ;; Bindings.
  (local-leader-def
    :keymaps 'TeX-mode-map
    "a" '(TeX-command-run-all :which-key "run all")
    "c" '(TeX-command-master :which-key "run command")
    "p" '(TeX-pin-region :which-key "pin region")
    "r" '(TeX-command-region :which-key "run command for region")
    "v" '(TeX-view :which-key "view"))
  ;; Do not use LaTeX-indent-tabular as it breaks complex tabular-type environments.
  (setq LaTeX-indent-environment-list
        '(("verbatim" current-indentation)
          ("verbatim*" current-indentation)
          ("filecontents" current-indentation)
          ("filecontents*" current-indentation)
          ("tabular")
          ("tabular*")
          ("align")
          ("align*")
          ("array")
          ("eqnarray")
          ("eqnarray*")
          ("displaymath")
          ("equation")
          ("equation*")
          ("picture")
          ("tabbing"))))

(use-package cdlatex
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (org-mode . turn-on-org-cdlatex))
  :config
  (general-define-key
   :keymaps 'cdlatex-mode-map
   ;; Use smartparens to do parens pairing.
   "(" nil
   "{" nil
   "[" nil
   "|" nil
   "<" nil
   "TAB" nil
   "<tab>" nil)
  ;; TODO: make cdlatex active in insert mode only.
  (general-define-key
   :states 'insert
   :keymaps 'cdlatex-mode-map
   "TAB" #'cdlatex-tab
   "<tab>" #'cdlatex-tab)
  (setq cdlatex-use-dollar-to-ensure-math nil)
  (setq cdlatex-command-alist
        '(("cfr" "Insert \\cfrac{}{}"
           "\\cfrac{?}{}" cdlatex-position-cursor nil nil t)
          ("dfr" "Insert \\dfrac{}{}"
           "\\dfrac{?}{}" cdlatex-position-cursor nil nil t)
          ("xl" "Insert \\xleftarrow{}"
           "\\xleftarrow{?}" cdlatex-position-cursor nil nil t)
          ("xr" "Insert \\xrightarrow{}"
           "\\xrightarrow{?}" cdlatex-position-cursor nil nil t)
          ("la" "Insert left angle spinor"
           "\\langle ? \\rvert" cdlatex-position-cursor nil nil t)
          ("La" "Insert dynamically-sized left angle spinor"
           "\\left\\langle ? \\right\\rvert" cdlatex-position-cursor nil nil t)
          ("lb" "Insert left square spinor"
           "[ ? \\rvert" cdlatex-position-cursor nil nil t)
          ("Lb" "Insert dynamically-sized left square spinor"
           "\\left[ ? \\right\\rvert" cdlatex-position-cursor nil nil t)
          ("ra" "Insert right angle spinor"
           "\\lvert ? \\rangle" cdlatex-position-cursor nil nil t)
          ("Ra" "Insert dynamically-sized right angle spinor"
           "\\left\\lvert ? \\right\\rangle" cdlatex-position-cursor nil nil t)
          ("rb" "Insert right square spinor"
           "\\lvert ? ]" cdlatex-position-cursor nil nil t)
          ("Rb" "Insert dynamically-sized right square spinor"
           "\\left\\lvert ? \\right]" cdlatex-position-cursor nil nil t)
          ("lra" "Insert angle spinor bracket"
           "\\langle ? \\rangle" cdlatex-position-cursor nil nil t)
          ("lrb" "Insert square spinor bracket"
           "[ ? ]" cdlatex-position-cursor nil nil t)))
  (setq cdlatex-math-modify-alist
        '(( ?b "\\bm" nil t nil nil )
          ( ?B "\\mathbf" nil t nil nil )
          ( ?s "\\mathscr" nil t nil nil )
          ( ?t "\\text" nil t nil nil )
          ( ?= "\\mathbb" nil t nil nil )
          ( ?/ "\\slashed" nil t nil nil )))
  (setq cdlatex-math-symbol-alist
        '(( ?F ("\\Phi"))
          ( ?+ ("\\oplus" "\\cup"))
          ( ?* ("\\otimes" "\\times"))))
  ;; TODO: Rewrite with advice-add.
  (defadvice cdlatex-sub-superscript (around my-cdlatex-sub-superscript activate)
    "Use only in math-mode."
    (if (texmathp)
        ad-do-it
      (insert (event-basic-type last-command-event)))))

(use-package evil-tex
  :hook (LaTeX-mode . evil-tex-mode))

(use-package reftex ; built-in
  :hook (TeX-mode . reftex-mode))
