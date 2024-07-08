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
  :custom
  (org-use-fast-todo-selection 'expert) ; no popup window
  :init
  ;; Remove this setting after
  ;; https://github.com/emacs-evil/evil/issues/1630 gets fixed.
  (setq org-fold-core-style 'overlays)
  :config
  ;; Paths.
  (setq org-directory my/org-directory
        org-agenda-files
        (mapcar (lambda (name) (expand-file-name name my/org-directory))
                '("gtd.org" "calendar.org" "inbox.org" "tickler.org")))
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
        org-agenda-tags-column 'auto
        org-agenda-skip-scheduled-if-deadline-is-shown 'not-today
        org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %17TIMESTAMP_IA")
  ;; Agenda.
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
            (org-agenda-start-with-log-mode '(closed clock state))))
          ("g" "Get Things Done (GTD)"
           ((agenda
             ""
             ((org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline))
              (org-agenda-span 'day)
              (org-deadline-warning-days 0)))
            (todo
             "NEXT"
             ((org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline))
              (org-agenda-prefix-format "  %i %-12:c [%e] ")
              (org-agenda-overriding-header "\nTasks\n")))
            (agenda
             nil
             ((org-agenda-entry-types '(:deadline))
              (org-agenda-format-date "")
              (org-deadline-warning-days 7)
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
              (org-agenda-overriding-header "\nDeadlines")))
            (tags-todo
             "inbox"
             ((org-agenda-prefix-format "  %?-12t% s")
              (org-agenda-overriding-header "\nInbox\n")))
            (tags
             "CLOSED>=\"<today>\""
             ((org-agenda-overriding-header "\nCompleted today\n")))))))
  (add-to-list
   'display-buffer-alist
   '("\\*Org Agenda\\*"
     ;; (display-buffer-in-tab display-buffer-reuse-mode-window)
     ;; (tab-name . "gtd")
     ;; (dedicated . side)
     ;; (side . left)))
     (display-buffer-in-tab display-buffer-reuse-window display-buffer-in-direction)
     (tab-name . "gtd")
     (direction . leftmost)))
  (defun my/display-buffer-org-agenda-file-p (buffer-name action)
    "Return non-nil, if BUFFER-NAME is associated with an agenda file."
    (with-current-buffer buffer-name
      (and (derived-mode-p 'org-mode) (org-agenda-file-p))))
  (add-to-list
   'display-buffer-alist
   '(my/display-buffer-org-agenda-file-p
     (display-buffer-in-tab display-buffer-reuse-mode-window display-buffer-in-direction)
     (tab-name . "gtd")
     (direction . rightmost)))
  ;; Org notes.
  (add-to-list
   'display-buffer-alist
   '("\*Org Note\*"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 0)))
  (advice-add
   #'org-add-log-note
   :around #'my/suppress-delete-other-windows)
  ;; Refile.
  (setq org-refile-targets
        '(("gtd.org" :maxlevel . 3)
          ("someday.org" :level . 1)
          ("tickler.org" :level . 1)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
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

  (setq org-highlight-latex-and-related nil
        org-latex-packages-alist
        '(("" "mathtools")
          ("" "slashed")
          ("" "arev"))
        org-preview-latex-default-process 'dvisvgm
        org-preview-latex-image-directory ".ltximg/")

  (plist-put org-format-latex-options :scale 1.0)
  (plist-put org-format-latex-options :background "Transparent")

  (defun my/org-latex-filter-nbsp (text backend info)
    "Ensure non-breaking spaces are properly handled in LaTeX export."
    (when (org-export-derived-backend-p backend 'latex)
      (replace-regexp-in-string " " "~" text)))

  (with-eval-after-load 'ox
    (add-to-list 'org-export-filter-plain-text-functions
                 #'my/org-latex-filter-nbsp))

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
    "a" '("attachments" . org-attach)
    "c" '("clock" . (keymap))
    "ci" '("in" . org-clock-in)
    "co" '("out" . org-clock-out)
    "d" '("date" . (keymap))
    "da" '("active" . org-time-stamp)
    "dd" '("deadline" . org-deadline)
    "di" '("inactive" . org-time-stamp-inactive)
    "ds" '("schedule" . org-schedule)
    "D" '("archive subtree" . org-archive-subtree-default)
    "e" '("effort" . org-set-effort)
    "i" '("tags" . org-set-tags-command)
    "l" '("insert link" . org-insert-link)
    "r" '("refile" . org-refile)
    "s" '("todo state" . org-todo)
    "t" '("toggle" . (keymap))
    "td" '("dates" . org-toggle-time-stamp-overlays)
    "te" '("entities" . org-toggle-pretty-entities)
    "ti" '("images" . org-toggle-inline-images)
    "tl" '("links" . org-toggle-link-display)
    "tt" '("latex" . org-latex-preview)
    "u" '("reveal" . org-fold-reveal)
    "y" '("copy as latex" . my/org-copy-as-latex)
    "'" '("edit" . org-edit-special)))

(use-package org-agenda
  :straight nil ;; part of org
  :custom
  (org-agenda-hide-tags-regexp ".")
  :config
  (add-to-list
   'display-buffer-alist
   '("\*Agenda Commands\*"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 0)))
  (advice-add #'org-agenda-get-restriction-and-command :around #'my/suppress-delete-other-windows))

(use-package org-archive
  :straight nil ;; part of org
  :custom
  (org-archive-file-header-format
   "#+FILETAGS: ARCHIVE\nArchived entries from file %s\n")
  (org-archive-location "archive/%s_archive::")
  (org-archive-subtree-add-inherited-tags t))

(use-package org-capture
  :straight nil ;; part of org
  :commands (org-capture)
  :config
  (setq org-capture-templates
        `(("i" "Inbox" entry (file "inbox.org")
           "* %?\n:PROPERTIES:\n:CREATED:  %U\n:END:")
          ("m" "Inbox [email]" entry (file "inbox.org")
           ,(concat
             "* TODO Process %a from %:fromname %? :@email:\n"
             ":PROPERTIES:\n:CREATED:  %U\n:END:"))
          ("t" "Tickler" entry (file "tickler.org")
           ,(concat
             "* TODO %?\nSCHEDULED: %^t\n"
             ":PROPERTIES:\n:CREATED:  %U\n:END:"))
          ("c" "Calendar" entry (file "calendar.org")
           "* %?\n%^t%^{TIMEZONE}p")
          ("l" "Link" entry (file "inbox.org")
           "* %a%?\n:PROPERTIES:\n:CREATED:  %U\n:END:")
          ("d" "Diary" entry (file+olp+datetree "reference/diary.org")
           "* %?\n:PROPERTIES:\n:CREATED:  %U\n:END:")
          ("W" "Web browser link" entry
           (file+headline "gtd.org" "Tasks")
           ,(concat
             "* NEXT [#C] %?Read %:annotation :@computer:@read:\n"
             ":PROPERTIES:\n:CREATED:  %U\n:END:"))))
  (add-to-list
   'display-buffer-alist
   '("\*Org Select\*"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 0)))
  (add-to-list
   'display-buffer-alist
   '("CAPTURE-.*\.org"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 0)))
  (advice-add
   #'org-capture-place-template
   :around #'my/suppress-delete-other-windows)
  (local-leader-def
    :keymaps 'org-capture-mode-map
    "r" '("refile" . org-capture-refile)))

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

(use-package ox
  :straight nil ;; part of org
  :config
  (add-to-list
   'display-buffer-alist
   '("\*Org Export Dispatcher\*"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 0)))
  (advice-add
   #'org-export--dispatch-ui
   :around #'my/suppress-delete-other-windows))

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
    "t" '("toggle" . (keymap))
    "ta" '("archives" . my/org-agenda-toggle-archives)
    "te" '("entry text" . org-agenda-entry-text-mode)))

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
  ;; Sometimes org-export fails to resolve org-id links. The following
  ;; function fixes the issue (see
  ;; https://dev.to/devteam/resolving-an-unable-to-resolve-link-error-for-org-mode-in-emacs-2n1f
  ;; for details):
  (defun my/org-roam-rebuild-cache ()
    "Rebuild the `org-mode' and `org-roam' cache."
    (interactive)
    (org-id-update-id-locations)
    (org-roam-db-sync)
    (org-roam-update-org-id-locations))
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
    "tv" '("vertical align" . valign-mode))
  :config
  (setq valign-fancy-bar t))

;;; Beautifying

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-prettify-item-bullets t
        org-superstar-item-bullet-alist '((?+ . ?➤) (?- . ?•))))
