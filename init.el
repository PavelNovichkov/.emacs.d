;;; init.el --- Emacs startup file -*- lexical-binding: t; -*-

(require 'core (concat user-emacs-directory "core/core"))

(dolist (module-name '("secrets"
                       "ui/color-theme"
                       "evil/plugins"
                       "emacs/dired"
                       "emacs/ediff"
                       "emacs/helpful"
                       "emacs/ibuffer"
                       "emacs/outline"
                       "emacs/paradox"
                       "emacs/smartparens"
                       "emacs/tramp"
                       "emacs/winner"
                       "lang/latex"
                       "layouts"
                       "projects"
                       "ui/evil-goggles"
                       "app/irc"
                       "app/ledger"
                       "app/mail"
                       "app/organizer"
                       "app/rss"
                       "tools/bibliography"
                       "tools/dictionary"
                       "tools/link-hint"
                       "tools/magit"
                       "tools/pdf"
                       "tools/spellcheck"
                       "tools/tldr"
                       "bindings"))
  (load (concat user-emacs-directory "modules/" module-name) nil t))
