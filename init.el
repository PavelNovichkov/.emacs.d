;;; init.el --- Emacs startup file -*- lexical-binding: t; -*-

(load (concat user-emacs-directory "core/core") nil t)

(dolist (module-name '("secrets"
                       "ui/frame"
                       "ui/color-theme"
                       "evil/plugins"
                       "emacs/dired"
                       "emacs/ediff"
                       "emacs/eldoc"
                       "emacs/gpg"
                       "emacs/helpful"
                       "emacs/ibuffer"
                       "emacs/outline"
                       "emacs/recentf"
                       "emacs/smartparens"
                       "emacs/tramp"
                       "emacs/winner"
                       "lang/elisp"
                       "lang/latex"
                       "lang/cc"
                       "lang/wolfram"
                       "layouts"
                       "projects"
                       "ui/evil-goggles"
                       "ui/hl-todo"
                       "ui/olivetti"
                       "ui/pulsar"
                       "app/beeminder"
                       "app/irc"
                       "app/ledger"
                       "app/mail"
                       "app/organizer"
                       "app/system-packages"
                       "app/rss"
                       "app/terminal"
                       "tools/activity-watch"
                       "tools/bibliography"
                       "tools/dictionary"
                       "tools/link-hint"
                       "tools/lsp"
                       "tools/pass"
                       "tools/pdf"
                       "tools/spellcheck"
                       "tools/tldr"
                       "tools/vcs"
                       "bindings"))
  (load (concat user-emacs-directory "modules/" module-name) nil t))
