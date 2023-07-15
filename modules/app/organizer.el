;;; app/organizer.el -*- lexical-binding: t; -*-

(defconst my/org-directory (file-truename "~/data/org")
  "Org directory.")
(defconst my/slip-box-directory (expand-file-name "slip-box" my/org-directory)
  "Slip box directory.")

(setq calendar-week-start-day 1)
(set-time-zone-rule "CET")

;; TODO: refactor.
(use-package org
  ;; pre-loaded in core-packages
  :straight nil
  :bind (:map org-mode-map
              ("C-c [" . nil)
              ("C-c ]" . nil))
  :commands (org-agenda
             org-store-link)
  :config
  ;; Paths.
  (setq org-directory my/org-directory
        org-agenda-files
        (mapcar (lambda (name) (expand-file-name name my/org-directory))
                '("gtd.org" "calendar.org" "tickler.org")))
  ;; Variables.
  (add-to-list 'org-modules 'org-habit t)
  (setq org-enforce-todo-dependencies t
        org-special-ctrl-a/e t
        org-M-RET-may-split-line '((default . nil))
        org-log-into-drawer t
        org-src-fontify-natively t
        org-src-window-setup 'current-window
        org-edit-src-content-indentation 0  ;; another option is (setq org-src-preserve-indentation t)
        org-list-allow-alphabetical t
        org-startup-folded t
        org-startup-indented t
        org-catch-invisible-edits 'show-and-error
        org-log-done 'time
        org-tags-column 0
        org-agenda-window-setup 'current-window
        org-agenda-tags-column -150
        org-agenda-skip-scheduled-if-deadline-is-shown 'not-today
        org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %17TIMESTAMP_IA")
  ;; Agenda.
  (setq org-agenda-buffer-name "Org Agenda") ; Remove asterisks to be included in iflipb list.
  (setq org-agenda-custom-commands
        '(("v" "Main view"
           ((agenda
             ""
             ((org-agenda-overriding-header "Schedule:")
              (org-agenda-span 'day)
              (org-agenda-start-on-weekday nil)))
            (tags-todo
             "-CANCELED-ARCHIVE/!NEXT"
             ((org-agenda-overriding-header "Next tasks:")))
            (tags-todo
             "-CANCELED-ARCHIVE/!WAITING"
             ((org-agenda-overriding-header "Waiting list:"))))
           ((org-agenda-prefix-format
             '((tags . "  %-23(truncate-string-to-width (or (cadr (org-get-outline-path)) \"Task\") 23 nil nil \"...\") ")))
            (org-agenda-start-with-log-mode '(closed clock state))))))
  ;; Refile.
  (setq org-refile-targets
        '(("gtd.org" :maxlevel . 3)
          ("someday.org" :level . 1)
          ("tickler.org" :level . 1)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  ;; Archive.
  (setq org-archive-location "archive/%s_archive::")
  (setq org-archive-file-header-format "#+FILETAGS: ARCHIVE\nArchived entries from file %s\n")
  ;; Clocking.
  (setq org-clock-heading-function
        (lambda ()
          (let ((str (nth 4 (org-heading-components))))
            (if (> (length str) 25)
                (concat (substring str 0 22) "...") str))))
  ;; CREATED property.
  (defun my/org-set-created-property ()
    "Set a CREATED property on the entry giving the creation time"
    (interactive)
    (org-set-property "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]")))
  ;; File type associations.
  (setq org-file-apps
        '((auto-mode . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . "xdg-open %s")
          ("\\.pdf\\'" . default)))
  ;; Org-babel.
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((latex . t)
                                 (shell . t)))
  ;; Images.
  (setq org-image-actual-width `(,(/ (display-pixel-width) 4)))
  ;; LaTeX.
  (setq org-preview-latex-default-process 'dvipng
        org-preview-latex-image-directory ".ltximg/")
  (setq org-latex-packages-alist
        '(("" "mathrsfs")
          ("" "dsfont")
          ("" "mathtools")
          ("" "slashed")
          ("scaled=0.98" "XCharter")
          ("charter,scaled=1.05,smallerops,upint,vvarbb" "newtxmath")))
  (plist-put org-format-latex-options :scale 1.6)
  (setq org-highlight-latex-and-related '(native))
  (defun my/org-copy-as-latex ()
    "Export region to LaTeX, and copy it to the clipboard."
    (interactive)
    (save-window-excursion
      (let* ((buf (org-export-to-buffer 'latex "*Formatted Copy*" nil nil nil t))
             (latex (with-current-buffer buf (buffer-string))))
        (with-current-buffer buf
          (copy-region-as-kill (point-min) (point-max)))
        (kill-buffer buf)))
    (setq deactivate-mark t))
  ;; Bindings.
  (local-leader-def
    :keymaps 'org-mode-map
    "a" '(org-attach :which-key "attachments")
    "c" '(:ignore t :which-key "clock")
    "ci" '(org-clock-in :which-key "in")
    "co" '(org-clock-out :which-key "out")
    "d" '(:ignore t :which-key "date")
    "da" '(org-time-stamp :which-key "active")
    "dd" '(org-deadline :which-key "deadline")
    "di" '(org-time-stamp-inactive :which-key "inactive")
    "ds" '(org-schedule :which-key "schedule")
    "D" '(org-archive-subtree-default :which-key "archive subtree")
    "e" '(org-set-effort :which-key "effort")
    "i" '(org-set-tags-command :which-key "tags")
    "l" '(org-insert-link :which-key "insert link")
    "r" '(org-refile :which-key "refile")
    "s" '(org-todo :which-key "todo state")
    "t" '(:ignore t :which-key "toggle")
    "td" '(org-toggle-time-stamp-overlays :which-key "dates")
    "te" '(org-toggle-pretty-entities :which-key "entities")
    "ti" '(org-toggle-inline-images :which-key "images")
    "tl" '(org-toggle-link-display :which-key "links")
    "tt" '(org-latex-preview :which-key "latex")
    "y" '(my/org-copy-as-latex :which-key "copy as latex")
    "'" '(org-edit-special :which-key "edit")))

(use-package org-capture
  :straight nil ;; part of org
  :commands (org-capture)
  :config
  (setq org-capture-templates
        '(("i" "Inbox" entry (file "inbox.org")
           "* %?\n:PROPERTIES:\n:CREATED:  %U\n:END:")
          ("t" "Tickler" entry (file "tickler.org")
           "* TODO %?\nSCHEDULED: %^t\n:PROPERTIES:\n:CREATED:  %U\n:END:")
          ("c" "Calendar" entry (file "calendar.org")
           "* %?\n%^t")
          ("l" "Link" entry (file "inbox.org")
           "* %a%?\n:PROPERTIES:\n:CREATED:  %U\n:END:")
          ("d" "Diary" entry (file+olp+datetree "reference/diary.org")
           "* %?\n:PROPERTIES:\n:CREATED:  %U\n:END:")
          ("W" "Web browser link" entry (file+headline "gtd.org" "Tasks")
           "* NEXT [#C] %?Read %:annotation :@computer:@read:\n:PROPERTIES:\n:CREATED:  %U\n:END:")))
  (local-leader-def
    :keymaps 'org-capture-mode-map
    "r" '(org-capture-refile :which-key "refile")))

;; org-protocol allows to capture content from a web browser using bookmarklets:
;; - store link: "javascript:location.href='org-protocol://store-link?url='+encodeURIComponent(location.href);"
;; - add webpage to reading list: "javascript:location.href='org-protocol://capture?template=W&url='+encodeURIComponent(window.location.href)+'&title='+encodeURIComponent(document.title)+'&body='+encodeURIComponent(window.getSelection());"
;; - add web note: "javascript:location.href='org-protocol://roam-ref?template=w&ref='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)"
(use-package org-protocol
  :straight nil ;; part of org
  :init
  (defun my/org-protocol-lazy-load (orig-fun &rest args)
    "Lazy load org-protocol when visited file matches \":/+\".
When one of the visited files matches \":/+\", remove this advice and
apply the version of `server-visited-files' advised by `org-protocol'.
Otherwise, use the original version of `server-visited-files'."
    (let ((files (car args)))
      (catch 'greedy
        (dolist (var files)
          (when (string-match-p ":/+" (car var))
            (advice-remove #'server-visit-files #'my/org-protocol-lazy-load)
            (require 'org-protocol)
            (throw 'greedy (apply #'server-visit-files args))))
        (apply orig-fun args))))
  (advice-add #'server-visit-files :around #'my/org-protocol-lazy-load))

;;; Integration with evil

(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :init
  (setq evil-org-key-theme '(insert textobjects additional calendar))
  :config
  (evil-org-set-key-theme)
  (evil-normalize-keymaps))

(use-package evil-org-agenda
  :straight nil ;; part of evil-org
  :demand :after org-agenda
  :config
  (evil-org-agenda-set-keys)
  (general-unbind :states 'motion :keymaps 'org-agenda-mode-map "SPC" "<delete>" "<backspace>")
  (general-unbind :keymaps 'org-agenda-mode-map "SPC" "<delete>" "<backspace>")
  (defun my/org-agenda-toggle-archives ()
    "Toggle inclusion of archives in Org agenda."
    (interactive)
    (org-agenda-archives-mode t))
  (local-leader-def
    :keymaps 'org-agenda-mode-map
    "t" '(:ignore t :which-key "toggle")
    "ta" '(my/org-agenda-toggle-archives :which-key "archives")
    "te" '(org-agenda-entry-text-mode :which-key "entry text")))

;;; Attachments

(use-package org-attach
  :straight nil ;; part of org
  :demand :after org
  :config
  (setq org-attach-id-dir (expand-file-name "db" my/org-directory))
  (setq org-attach-auto-tag "attach"))

(use-package org-download
  :demand :after org
  :config
  (setq org-download-method 'attach)
  (setq org-download-timestamp "")
  ;; Redefine org-download-insert-link in the following ways:
  ;; - do not insert link annotation
  ;; - use absolute path to attachment
  ;; - do not turn on inline images display
  (defun my/org-download-insert-link (link filename)
    (if (looking-back "^[ \t]+" (line-beginning-position))
        (delete-region (match-beginning 0) (match-end 0))
      (newline))
    (insert
     (concat
      (if (= org-download-image-html-width 0)
          ""
        (format "#+attr_html: :width %dpx\n" org-download-image-html-width))
      (if (= org-download-image-latex-width 0)
          ""
        (format "#+attr_latex: :width %dcm\n" org-download-image-latex-width))
      (format "[[attachment:%s]]" (file-name-nondirectory filename)))))
  (advice-add #'org-download-insert-link :override #'my/org-download-insert-link))

;;; Priorities

(use-package org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("🄰" "🄱" "🄲"))
  ;; A hack to make priority face in agenda the same as in org files
  ;; themselves.
  (setq org-priority-faces '((?A . (error org-priority))
                             (?B . (warning org-priority))
                             (?C . (success org-priority))))
  ;; Remove priority after completing the task.
  (defun my/remove-priority-when-task-done ()
    "Removes priority from a task if its state is DONE or CANCELED."
    (when (and (member org-state '("DONE" "CANCELED"))
               (string-match org-priority-regexp (org-get-heading t t nil)))
      (org-priority ?\ )))
  (add-hook 'org-after-todo-state-change-hook
            #'my/remove-priority-when-task-done))

;;; Slip-box aka Zettelkasten

(use-package org-roam
  :commands (org-roam-node-find org-roam-node-insert org-roam-capture)
  :init
  (setq org-roam-v2-ack t)
  ;; Org-roam extends org-protocol, so lazy-load it.
  (with-eval-after-load 'org-protocol
    (require 'org-roam-protocol))
  :config
  (setq org-roam-directory my/slip-box-directory
        ;; treat only files as notes
        org-roam-db-node-include-function #'org-before-first-heading-p
        org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head
                    "%<%Y%m%d%H%M%S>.org"
                    "#+title: ${title}\n\n- tags :: \n")
           :unnarrowed t)
          ("m" "meeting" plain "%?"
           :if-new (file+head
                    "meetings/%<%Y%m%d%H%M%S>.org"
                    "#+title: ${title}\n\n- tags :: \n- participants :: \n")
           :unnarrowed t)
          ("t" "talk" plain "%?"
           :if-new (file+head
                    "talks/%<%Y%m%d%H%M%S>.org"
                    "#+title: ${title}\n\n- tags :: \n- speaker :: \n")
           :unnarrowed t))
        org-roam-capture-ref-templates
        '(("w" "web" plain "%?"
           :if-new (file+head
                    "web/%<%Y%m%d%H%M%S>.org"
                    "#+title: ${title}\n\n- tags :: \n")
           :unnarrowed t)))
  (add-hook 'org-roam-capture-new-node-hook #'my/org-set-created-property)
  (org-roam-setup))

;;; Integration with Anki for spaced repetition

(use-package anki-editor
  :init
  (defconst my/anki-file (expand-file-name "anki.org" my/org-directory)))
  ;; TODO Add capture templates

;;; Password manager

(use-package org-password-manager
  :commands (org-password-manager-generate-password
             org-password-manager-get-password
             org-password-manager-get-username)
  :config
  (setq org-password-manager-scope
        (list (expand-file-name "reference/secrets.gpg" my/org-directory))))

;;; Pomodoro

(use-package org-pomodoro
  :commands org-pomodoro
  :config
  (setq alert-user-configuration
        '((((:category . "org-pomodoro")) libnotify nil)))
  (setq org-pomodoro-audio-player "paplay"
        org-pomodoro-long-break-frequency 10))

(use-package org-mru-clock
  :init
  (defun my/org-clock-files ()
    (list (expand-file-name "gtd.org" my/org-directory)))
  (defun my/org-entry-has-sci-tag-p ()
    (member "SCI" (org-get-tags nil t)))
  :custom
  (org-mru-clock-files #'my/org-clock-files)
  (org-mru-clock-format-function 'substring)
  (org-mru-clock-how-many 15)
  (org-mru-clock-include-entry-at-point t)
  (org-mru-clock-predicate #'my/org-entry-has-sci-tag-p)
  :config
  (add-hook 'minibuffer-setup-hook #'org-mru-clock-embark-minibuffer-hook))

;; (use-package org-mru-clock
;;   :config
;;   (defun my/org-clock-files ()
;;     (expand-file-name "gtd.org" my/org-directory))
;;   (defun my/org-entry-is-next-p ()
;;     (string= (org-get-todo-state) "NEXT"))
;;   (defun my/org-mru-clock--pomodoro (task)
;;     "Start or stop a pomodoro for a given TASK."
;;     (let ((marker (cdr task)))
;;       (with-current-buffer (org-base-buffer (marker-buffer marker))
;;         (org-with-wide-buffer
;;          (goto-char (marker-position marker))
;;          (org-pomodoro)))))
;;   (defun my/org-mru-clock--extend-pomodoro (task)
;;     "Extend the last pomodoro ignoring the given TASK."
;;     (org-pomodoro-extend-last-clock))
;;   (setq org-mru-clock-keep-formatting t
;;         org-mru-clock-how-many 5
;;         org-mru-clock-files #'my/org-clock-files
;;         org-mru-clock-predicate #'my/org-entry-is-next-p)
;;   ;; (ivy-add-actions 'org-mru-clock-in
;;   ;;                   '(("p" my/org-mru-clock--pomodoro "start/stop a pomodoro")
;;   ;;                     ("x" my/org-mru-clock--extend-pomodoro "extend last pomodoro")))
;;   ;; Use default sorting instead of prescient.
;;   ;; (cl-pushnew #'org-mru-clock-in (cdr ivy-prescient-sort-commands))
;;   )

;;; Table alignment

(use-package valign
  :straight (:host github :repo "casouri/valign")
  :commands valign-mode
  :init
  (local-leader-def
    :keymaps 'org-mode-map
    "tv" '(valign-mode :which-key "vertical align"))
  :config
  (setq valign-fancy-bar t))

;;; Beautifying

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-prettify-item-bullets t
        org-superstar-item-bullet-alist '((?+ . ?➤) (?- . ?•))))
