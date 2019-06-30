;;; app/rss.el -*- lexical-binding: t; -*-

(use-package elfeed
  :commands elfeed
  :config
  (setq-default elfeed-search-filter "@1-month-ago +unread "))

(use-package elfeed-org
  :demand :after elfeed
  :config
  (require 'org)
  (elfeed-org)
  (setq rmh-elfeed-org-files '("~/org/reference/emacs.org")))
