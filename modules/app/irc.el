;;; app/irc.el -*- lexical-binding: t; -*-

(use-package erc ; built-in
  :straight nil
  :commands erc
  :config
  (setq erc-hide-list '("JOIN" "PART" "QUIT")))
