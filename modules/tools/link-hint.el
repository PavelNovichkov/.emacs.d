;;; tools/link-hint.el -*- lexical-binding: t; -*-

(use-package link-hint
  :config
  (link-hint-define-type 'org-link
    :next #'link-hint--next-org-link
    :at-point-p #'link-hint--org-link-at-point-p
    :vars '(org-mode org-agenda-mode)
    :open #'link-hint--open-org-link
    :open-multiple t
    :copy #'kill-new))
