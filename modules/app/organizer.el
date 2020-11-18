;;; app/organizer.el -*- lexical-binding: t; -*-

(setq calendar-week-start-day 1)

;; TODO: refactor.
(use-package org
  :straight (:host github
             :repo "emacs-straight/org-mode"
             :branch "maint" ;; use latest stable version
             :local-repo "org"
             :files ("*.el" "lisp/*.el" "contrib/lisp/*.el"))
  :bind (:map org-mode-map
              ("C-c [" . nil)
              ("C-c ]" . nil))
  :commands (org-agenda
             org-store-link)
  :config
  ;; Paths.
  (setq org-directory (file-truename "~/org")
        org-agenda-files
        (mapcar (lambda (name) (expand-file-name name org-directory))
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
        org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %17TIMESTAMP_IA"
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "|" "DONE(d)" "CANCELED(c@)"))
        org-tag-alist
        '((:startgroup . nil)
          ("@home" . ?h) ("@office" . ?o) ("@city" . ?c) ("@everywhere" . ?e) ("@supermarket" . ?u)
          (:endgroup . nil)
          ("@computer" . ?k) ("@email" . ?m) ("@study" . ?s) ("@read" . ?r) ("@phone" . ?p)
          ("physics") ("emacs") ("music") ("house") ("health") ("linux")
          ("academia") ("video") ("finance") ("STP")))
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
  (setq org-preview-latex-default-process 'dvisvgm
        org-preview-latex-image-directory ".ltximg/")
  (setq org-latex-packages-alist '(("" "mathrsfs") ("" "dsfont") ("" "mathtools")))
  (plist-put org-format-latex-options :scale 1.4)
  (setq org-highlight-latex-and-related '(latex))
  ;; Bindings.
  (local-leader-def
    :keymaps 'org-mode-map
    "a" '(org-attach :which-key "attachments")
    "d" '(org-deadline :which-key "deadline")
    "D" '(org-archive-subtree-default :which-key "archive subtree")
    "e" '(org-set-effort :which-key "effort")
    "f" '(counsel-org-file :which-key "browse file attachments")
    "l" '(org-insert-link :which-key "insert link")
    "q" '(counsel-org-tag :which-key "tags")
    "r" '(org-refile :which-key "refile")
    "s" '(org-schedule :which-key "schedule")
    "t" '(:ignore t :which-key "toggle")
    "te" '(org-toggle-pretty-entities :which-key "entities")
    "ti" '(org-toggle-inline-images :which-key "images")
    "tl" '(org-toggle-link-display :which-key "links")
    "tt" '(org-latex-preview :which-key "latex")
    "tT" '(org-toggle-time-stamp-overlays :which-key "timestamps"))
  (general-define-key
   :states 'motion :keymaps 'org-agenda-mode-map
   "cq" '(counsel-org-tag-agenda :which-key "tags")))

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
  :config
  (setq-default evil-org-key-theme '(insert textobjects additional calendar))
  ;; TODO: rewrite.
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)
              (evil-define-key 'normal evil-org-mode-map
                (kbd "<M-return>") (evil-org-define-eol-command
                                    org-meta-return)))))

(use-package evil-org-agenda
  :straight nil ;; part of evil-org
  :demand :after org-agenda
  :config
  (evil-org-agenda-set-keys))

;;; Attachments

(use-package org-attach
  :straight nil ;; part of org
  :demand :after org
  :config
  (setq org-attach-id-dir "~/org/db")
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
  (setq org-fancy-priorities-list '("HIGH" "MID " "LOW "))
  ;; A hack to make priority face in agenda the same as in org files
  ;; themselves.
  (setq org-priority-faces '((?A . (:inherit font-lock-keyword-face))
                             (?B . (:inherit font-lock-keyword-face))
                             (?C . (:inherit font-lock-keyword-face))))
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
  :commands (org-roam-insert org-roam-find-file org-roam)
  :init
  ;; Org-roam extends org-protocol, so lazy-load it.
  (with-eval-after-load 'org-protocol
    (require 'org-roam-protocol))
  ;; Pretend org-ref is installed so that citation links work.
  (defvar org-ref-cite-types '("cite"))
  (defun org-ref-split-and-strip-string (string)
    (split-string string ","))
  (provide 'org-ref)
  :config
  (setq org-roam-directory (expand-file-name "slip-box" org-directory)
        org-roam-completion-system 'ivy
        org-roam-capture-templates
        '(("d" "default" plain #'org-roam-capture--get-point
           "%?"
           :file-name "%<%Y%m%d%H%M%S>"
           :head "#+TITLE: ${title}\n#+CREATED:  %U\n\n- tags :: \n"
           :unnarrowed t
           :immediate-finish)
          ("t" "talk" plain #'org-roam-capture--get-point
           "%?"
           :file-name "talks/%<%Y%m%d%H%M%S>"
           :head "#+TITLE: ${title}\n#+CREATED:  %U\n\n- tags :: \n- speaker :: \n"
           :unnarrowed t
           :immediate-finish))
        org-roam-capture-ref-templates
        '(("w" "web" plain #'org-roam-capture--get-point
           "%?"
           :file-name "web/%<%Y%m%d%H%M%S>"
           :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n#+CREATED:  %U\n\n- source :: ${ref}\n- tags :: \n"
           :unnarrowed t
           :immediate-finish)))
  (org-roam-mode))

;;; Password manager

(use-package org-password-manager
  :commands (org-password-manager-generate-password
             org-password-manager-get-password
             org-password-manager-get-username)
  :config
  (setq org-password-manager-scope '("~/org/reference/secrets.gpg")))

;;; Pomodoro

(use-package org-pomodoro
  :commands org-pomodoro
  :config
  (setq alert-user-configuration
        '((((:category . "org-pomodoro")) libnotify nil))))

(use-package org-mru-clock
  :config
  (defun my/org-clock-files ()
    (list "~/org/gtd.org" "~/org/tickler.org"))
  (defun my/org-entry-is-next-p ()
    (string= (org-get-todo-state) "NEXT"))
  (defun my/org-mru-clock--pomodoro (task)
    "Start or stop a pomodoro for a given TASK."
    (let ((marker (cdr task)))
      (with-current-buffer (org-base-buffer (marker-buffer marker))
        (org-with-wide-buffer
         (goto-char (marker-position marker))
         (org-pomodoro)))))
  (defun my/org-mru-clock--extend-pomodoro (task)
    "Extend the last pomodoro ignoring the given TASK."
    (org-pomodoro-extend-last-clock))
  (setq org-mru-clock-completing-read #'ivy-completing-read
        org-mru-clock-keep-formatting t
        org-mru-clock-how-many 5
        org-mru-clock-files #'my/org-clock-files
        org-mru-clock-predicate #'my/org-entry-is-next-p)
  (ivy-add-actions 'org-mru-clock-in
                    '(("p" my/org-mru-clock--pomodoro "start/stop a pomodoro")
                      ("x" my/org-mru-clock--extend-pomodoro "extend last pomodoro")))
  ;; Use default sorting instead of prescient.
  (cl-pushnew #'org-mru-clock-in (cdr ivy-prescient-sort-commands)))
