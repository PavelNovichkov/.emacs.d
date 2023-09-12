;;; app/rss.el -*- lexical-binding: t; -*-

(use-package elfeed
  :commands elfeed
  :custom
  (elfeed-search-filter "@1-month-ago +unread ")
  (elfeed-search-date-format '("%d/%m/%Y" 10 :left))
  (elfeed-sort-order 'ascending))

(use-package elfeed-org
  :demand :after elfeed
  :custom
  (rmh-elfeed-org-files
   (list (expand-file-name "reference/emacs.org" my/org-directory)))
  :config
  (elfeed-org))
