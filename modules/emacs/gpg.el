;;; emacs/gpg.el -*- lexical-binding: t; -*-

(require 's)

;; Setup GPG-agent for SSH
(setenv "SSH_AUTH_SOCK"
        (s-trim (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket")))
(shell-command "gpgconf --launch gpg-agent")
