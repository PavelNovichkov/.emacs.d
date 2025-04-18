;;; app/mail.el -*- lexical-binding: t; -*-

(use-package mu4e
  :straight nil
  :ensure-system-package mu
  :commands mu4e
  :config

  ;;; Core

  (setq
   ;; retrieve mail with mbsync
   mu4e-get-mail-command "timeout 120 mbsync -q -a"
   ;; send mail with msmtp
   sendmail-program "/usr/bin/msmtp"
   message-send-mail-function #'message-send-mail-with-sendmail
   message-sendmail-envelope-from 'header ; use From field instead of real address
   message-sendmail-f-is-evil t
   message-kill-buffer-on-exit t
   ;; move mail
   mu4e-change-filenames-when-moving t) ; mbsync specific

  ;; Move to trash without 'trashed' flag (otherwise server deletes
  ;; message completely), see https://github.com/djcb/mu/issues/1136.
  (setf (alist-get 'trash mu4e-marks)
        (list :char '("d" . "▼")
              :prompt "dtrash"
              :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
              ;; No +T before -N so the message is not marked as IMAP-deleted:
              :action (lambda (docid msg target)
                        (mu4e--server-move docid (mu4e--mark-check-target target) "-N"))))

  ;;; Contexts

  (setq mu4e-contexts (--map (apply 'make-mu4e-context it) my/mail-contexts)
        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'always-ask)

  ;;; Main view

  (setq mu4e-split-view 'single-window
        ;; Do not use mu4e built-in completion system.
        mu4e-read-option-use-builtin nil
        mu4e-completing-read-function 'completing-read)

  (add-to-list
   'display-buffer-alist
   '("\\*mu4e-headers\\*"
     (display-buffer-in-tab)
     (tab-name . "mail")))
  
  ;;; Headers view

  (add-to-list
   'mu4e-header-info-custom
   '(:empty . (:name "Empty" :shortname "" :function (lambda (msg) ""))))

  (setq mu4e-headers-date-format "%d/%m/%y"
        mu4e-headers-fields
        '((:from-or-to . 24)
          (:empty . 2)
          (:subject . 45)
          (:empty . 2)
          (:human-date . 9))
        mu4e-headers-precise-alignment t
        mu4e-hide-index-messages t
        mu4e-search-include-related nil
        mu4e-search-threads nil
        mu4e-search-skip-duplicates nil
        mu4e-use-fancy-chars t)

  ;; Toggle sort direction (useful for inbox).
  (defun my/mu4e-headers-toggle-sort-direction ()
    "Toggle headers sort direction."
    (interactive)
    (setq mu4e-search-sort-direction
          (if (eq mu4e-search-sort-direction 'ascending)
              'descending 'ascending))
    (mu4e-search-rerun))

  (general-define-key
   :states 'normal
   :keymaps 'mu4e-headers-mode-map
   "gR" #'mu4e-update-mail-and-index
   "O" #'my/mu4e-headers-toggle-sort-direction)

  ;;; View messages

  (general-define-key
   :states 'normal
   :keymaps 'mu4e-view-mode-map
   "C-e" #'mu4e-view-detach)

  ;;; Compose messages

  (setq
   ;; Correctly set the From address when replying to an email.
   ;; TODO Should become unnecessary in mu4e 1.14, see here:
   ;; https://github.com/djcb/mu/issues/2708.
   message-alternative-emails #'mu4e-user-mail-address-p
   ;; Include date when citing.
   message-citation-line-function #'message-insert-formatted-citation-line
   message-citation-line-format "\n\n%f writes on %Y-%m-%d:\n"
   ;; Don't reply to myself.
   message-dont-reply-to-names #'mu4e-personal-or-alternative-address-p
   message-signature nil
   ;; Allow clients to reflow paragraphs.
   mu4e-compose-format-flowed t)

  ;; Respond to iCalendar invitations.
  (require 'mu4e-icalendar)

  ;;; Emacs integrations

  (setq
   ;; Make mu4e default mail client.
   mail-user-agent #'mu4e-user-agent
   read-mail-command #'mu4e
   ;; Do not show information in the modeline.
   mu4e-modeline-support nil)
  
  ;; Enable images.
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types)))


;;; Attach files from dired and embark


(use-package gnus-dired
  :straight nil ; built-in
  :commands gnus-dired-attach
  :init

  (local-leader-def
   :keymaps 'dired-mode-map
   "a" '("attach" . gnus-dired-attach))

  (defun my/embark-attach-file (file)
    "Attach FILE to an email message."
    (interactive "fAttach: ")
    (gnus-dired-attach (list file)))

  (general-define-key
   :keymaps 'embark-file-map
   "a" #'my/embark-attach-file)

  :config

  (require 'mu4e)

  ;; See https://www.djcbsoftware.nl/code/mu/mu4e/Dired.html.
  (defun my/gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
            (push (buffer-name buffer) buffers))))
      (nreverse buffers)))
  (advice-add #'gnus-dired-mail-buffers :override #'my/gnus-dired-mail-buffers)

  (setq gnus-dired-mail-mode 'mu4e-user-agent))


;;; Send HTML emails


(use-package org-mime)
