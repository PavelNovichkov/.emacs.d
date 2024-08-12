;;; emacs/calendar.el -*- lexical-binding: t; -*-

(use-package calendar ; built-in
  :straight nil

  :init
  (set-time-zone-rule "CET")

  :config
  (setopt
   calendar-date-style 'european
   calendar-left-margin 7
   calendar-minimum-window-height 10
   calendar-mode-line-format nil
   calendar-week-start-day 1)

  (add-hook 'calendar-today-visible-hook #'calendar-mark-today)

  (add-to-list
   'display-buffer-alist
   '("\\*Calendar\\*"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 0))))
