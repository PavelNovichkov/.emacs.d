;;; editor/autotype.el -*- lexical-binding: t; -*-


(use-package abbrev ; built-in
  :ensure nil

  :hook
  (org-mode . abbrev-mode)
  (LaTeX-mode . abbrev-mode)

  :config
  (setopt
   evil-want-abbrev-expand-on-insert-exit nil
   save-abbrevs nil))


(use-package tempel
  :ensure (:wait t)
  :demand :after abbrev

  :config
  (defun my/tempel--abbrev-hook (template)
    (tempel-insert template)
    t)

  (defun my/abbrev--template-args (template)
    "Generate `define-abbrev' arguments for a TEMPLATE."
    (let* ((abbrev (car template))
           (hook (make-symbol abbrev)))
      (fset hook (apply-partially #'my/tempel--abbrev-hook (cdr template)))
      (put hook 'no-self-insert t)
      (list abbrev "" hook)))

  (defun my/abbrev-templates (templates)
    "Generate `define-abbrev-table' arguments for TEMPLATES.
TEMPLATES is a list of templates; each template is a list whose first element is
the abbreviation string, and the remaining elements form a template using Tempel
syntax."
    (mapcar #'my/abbrev--template-args templates)))
