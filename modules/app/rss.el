;;; app/rss.el -*- lexical-binding: t; -*-

(use-package elfeed
  :commands elfeed
  :config
  (setq-default elfeed-search-filter "@1-month-ago +unread ")
  (setq elfeed-search-date-format '("%d/%m/%Y" 10 :left)
        elfeed-sort-order 'ascending))

(use-package elfeed-org
  :demand :after elfeed
  :config
  (require 'org)
  (elfeed-org)
  (setq rmh-elfeed-org-files '("~/org/reference/emacs.org")))
