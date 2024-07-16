;;; init.el --- Emacs startup file -*- lexical-binding: t; -*-

(load (concat user-emacs-directory "core/core") nil t)

(defconst my/modules
  '("secrets"
    "ui/frame"
    "ui/color-theme"
    "editor/autopair"
    "editor/autotype"
    "evil/plugins"
    "emacs/dired"
    "emacs/ediff"
    "emacs/eldoc"
    "emacs/gpg"
    "emacs/helpful"
    "emacs/ibuffer"
    "emacs/line-numbers"
    "emacs/outline"
    "emacs/recentf"
    "emacs/tramp"
    "emacs/whitespace"
    "emacs/word-wrap"
    "lang/elisp"
    "lang/latex"
    "lang/markdown"
    "lang/cc"
    "lang/wolfram"
    "lang/yaml"
    "layouts"
    "projects"
    "ui/evil-goggles"
    "ui/hl-line"
    "ui/hl-todo"
    "ui/olivetti"
    "ui/pulsar"
    "ui/windows"
    "app/beeminder"
    "app/irc"
    "app/ledger"
    "app/mail"
    "app/organizer"
    "app/system-packages"
    "app/rss"
    "app/terminal"
    "tools/bibliography"
    "tools/dictionary"
    "tools/link-hint"
    "tools/llm"
    "tools/lsp"
    "tools/pass"
    "tools/pdf"
    "tools/spellcheck"
    "tools/tldr"
    "tools/vcs"
    "bindings")
  "A list of modules to be loaded at startup.")

(dolist (module-name my/modules)
  ;; If a module fails to load, show a warning and continue.
  (condition-case the-error
      (load (concat user-emacs-directory "modules/" module-name) nil t)
    (error (warn (error-message-string the-error)))))
