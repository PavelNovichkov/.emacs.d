;;; emacs/gpg.el -*- lexical-binding: t; -*-

;; Setup GPG-agent for SSH
(setenv "SSH_AUTH_SOCK"
        (string-trim (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket")))
(shell-command "gpgconf --launch gpg-agent")
