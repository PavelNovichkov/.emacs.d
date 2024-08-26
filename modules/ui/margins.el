;;; ui/margins.el -*- lexical-binding: t; -*-

(use-package visual-fill-column
  :hook
  ((bibtex-mode
    calendar-mode
    conf-mode
    Custom-mode
    dictionary-mode
    dired-mode
    mu4e-view-mode
    org-agenda-mode
    text-mode
    prog-mode
    special-mode) . visual-fill-column-mode)

  :config
  (setopt
   visual-fill-column-center-text t
   visual-fill-column-enable-sensible-window-split t
   visual-fill-column-extra-text-width '(4 . 0) ; to account for line numbers
   visual-fill-column-fringes-outside-margins nil))
