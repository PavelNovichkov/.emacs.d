;;; app/mail.el -*- lexical-binding: t; -*-

(unless (string-match-p "gentoo\\|ARCH" operating-system-release)
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e"))

;; TODO refactor.
(use-package mu4e
  :straight nil
  :ensure-system-package mu
  :bind (:map mu4e-main-mode-map
              ("q" . quit-window)
              ("Q" . mu4e-quit)
              :map mu4e-view-mode-map
              ("M-q" . nil))
  :commands mu4e
  :config
  (setq mu4e-split-view 'single-window)  ;; No main view and splitting, show messages directly.
  (general-define-key :states 'normal :keymaps 'mu4e-headers-mode-map "gR" #'mu4e-update-mail-and-index)
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-maildir "~/.maildir")
  ;; Attachments.
  (setq mu4e-save-multiple-attachments-without-asking t
        mu4e-attachments-dir "~/Downloads")
  ;; Get mail with mbsync.
  (setq mu4e-get-mail-command "timeout 120 mbsync -q -a")
  ;; Send mail with smtpmail.
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq message-kill-buffer-on-exit t)
  ;; Don't reply to myself.
  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-user-mail-address-list
        `(,yandex-user-mail-address
          ,sissa-user-mail-address
          ,sissa-user-alternative-mail-address))
  ;; Hide messages.
  (setq mu4e-hide-index-messages t)
  ;; Complete with ivy.
  (setq mu4e-completing-read-function 'ivy-completing-read)
  ;; mbsync specific.
  (setq mu4e-change-filenames-when-moving t)
  ;; Move to trash without 'trashed' flag (otherwise server deletes
  ;; message completely), see https://github.com/djcb/mu/issues/1136.
  (setf (alist-get 'trash mu4e-marks)
        (list :char '("d" . "▼")
              :prompt "dtrash"
              :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
              ;; No +T before -N so the message is not marked as IMAP-deleted:
              :action (lambda (docid msg target)
                        (mu4e~proc-move docid (mu4e~mark-check-target target) "-N"))))
  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "personal"
             :match-func
             (lambda (msg)
               (when msg (string-prefix-p "/yandex" (mu4e-message-field msg :maildir))))
             :vars
             `((mu4e-sent-folder . "/yandex/Sent")
               (mu4e-drafts-folder . "/yandex/Drafts")
               (mu4e-trash-folder . "/yandex/Trash")
               (mu4e-refile-folder . "/yandex/Archive")
               (user-full-name . ,yandex-user-full-name)
               (user-mail-address . ,yandex-user-mail-address)
               (smtpmail-default-smtp-server . "smtp.yandex.com")
               (smtpmail-local-domain . "yandex.com")
               (smtpmail-smtp-server . "smtp.yandex.com")
               (smtpmail-stream-type . starttls)
               (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
               (smtpmail-smtp-service . 25)
               (mu4e-sent-messages-behavior . sent)
               (mu4e-maildir-shortcuts . (("/yandex/Inbox" . ?i)
                                          ("/yandex/Sent" . ?s)
                                          ("/yandex/Drafts" . ?d)
                                          ("/yandex/Trash" . ?t)
                                          ("/yandex/Archive" . ?a)))))
           ,(make-mu4e-context
             :name "work"
             :match-func
             (lambda (msg)
               (when msg (string-prefix-p "/sissa" (mu4e-message-field msg :maildir))))
             :vars
             `((mu4e-sent-folder . "/sissa/Sent")
               (mu4e-drafts-folder . "/sissa/Drafts")
               (mu4e-trash-folder . "/sissa/Trash")
               (mu4e-refile-folder . "/sissa/Archives/2020")
               (user-full-name . ,sissa-user-full-name)
               (user-mail-address . ,sissa-user-mail-address)
               (smtpmail-default-smtp-server . "smtp.sissa.it")
               (smtpmail-local-domain . "sissa.it")
               (smtpmail-smtp-server . "smtp.sissa.it")
               (smtpmail-stream-type . starttls)
               (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
               (smtpmail-smtp-service . 587)
               (mu4e-sent-messages-behavior . sent)
               (mu4e-maildir-shortcuts . (("/sissa/Inbox" . ?i)
                                          ("/sissa/Sent" . ?s)
                                          ("/sissa/Drafts" . ?d)
                                          ("/sissa/Trash" . ?t)
                                          ("/sissa/Archives/2020" . ?a)
                                          ("/sissa/Junk" . ?j)))))))
  (setq mu4e-context-policy 'pick-first)
  ;; No auto-fill when composing a message.
  (add-hook 'mu4e-compose-mode-hook #'turn-off-auto-fill)
  ;; Turn on word wrap when viewing a message.
  (add-hook 'mu4e-view-mode-hook #'turn-on-visual-line-mode)
  ;; Images.
  (setq mu4e-view-show-images t)
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  ;; Rich-text messages.
  (defun my/render-html-message ()
    (let ((dom (libxml-parse-html-region (point-min) (point-max))))
      (erase-buffer)
      (shr-insert-document dom)
      (goto-char (point-min))))
  (setq mu4e-html2text-command 'my/render-html-message)
  ;; View in browser.
  (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser) t)
  ;; Sort order.
  (setq mu4e-headers-show-threads nil)
  ;; Date format.
  (setq mu4e-headers-date-format "%d/%m/%Y")
  ;; Exclude related messages.
  (setq mu4e-headers-include-related nil)
  ;; Show duplicates.
  (setq mu4e-headers-skip-duplicates nil)
  ;; Fancy chars.
  (setq mu4e-use-fancy-chars t)
  ;; Stolen from Doom Emacs.
  (setq mu4e-headers-has-child-prefix '("+" . " ")
        mu4e-headers-empty-parent-prefix '("-" . " ")
        mu4e-headers-first-child-prefix '("\\" . " ")
        mu4e-headers-duplicate-prefix '("=" . " ")
        mu4e-headers-default-prefix '("|" . " ")
        mu4e-headers-draft-mark '("D" . " ")
        mu4e-headers-flagged-mark '("F" . " ")
        mu4e-headers-new-mark '("N" . " ")
        mu4e-headers-passed-mark '("P" . " ")
        mu4e-headers-replied-mark '("R" . " ")
        mu4e-headers-seen-mark '("S" . " ")
        mu4e-headers-trashed-mark '("T" . " ")
        mu4e-headers-attach-mark '("a" . " ")
        mu4e-headers-encrypted-mark '("x" . " ")
        mu4e-headers-signed-mark '("s" . " ")
        mu4e-headers-unread-mark '("u" . " ")))

(use-package org-mu4e
  :straight nil ;; FIXME add dependency on external package.
  :demand :after (org mu4e)
  :config
  (setq org-mu4e-convert-to-html t)
  ;; Store link to message if in header view, not to header query.
  (setq org-mu4e-link-query-in-headers-mode nil))
