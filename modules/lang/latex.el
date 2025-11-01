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
    "a" '("run all" . TeX-command-run-all)
    "c" '("run command" . TeX-command-master)
    "p" '("pin region" . TeX-pin-region)
    "r" '("run command for region" . TeX-command-region)
    "v" '("view" . TeX-view))
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

(use-package evil-tex
  :hook (LaTeX-mode . evil-tex-mode))

(use-package reftex ; built-in
  :hook (TeX-mode . reftex-mode))
