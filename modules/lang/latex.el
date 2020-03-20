;;; lang/latex.el -*- lexical-binding: t; -*-

(use-package tex
  :ensure auctex
  :config
  (setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "PDF Tools")
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'TeX-mode-hook #'TeX-source-correlate-mode)
  (setq-default TeX-engine 'luatex)
  ;; Bindings.
  (local-leader-def
    :keymaps 'TeX-mode-map
    "a" '(TeX-command-run-all :which-key "run all")
    "c" '(TeX-command-master :which-key "run command")
    "v" '(TeX-view :which-key "view")))

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
  (setq cdlatex-command-alist
        '(("cfr" "Insert \\cfrac{}{}"
           "\\cfrac{?}{}" cdlatex-position-cursor nil nil t)
          ("dfr" "Insert \\dfrac{}{}"
           "\\dfrac{?}{}" cdlatex-position-cursor nil nil t)))
  (setq cdlatex-math-modify-alist
        '(( ?s "\\mathscr" nil t nil nil )
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
