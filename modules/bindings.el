;;; bindings.el -*- lexical-binding: t; -*-

;;; Commands to be bound

;; Adapted from
;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html.
(defun my/narrow-or-widen-dwim (arg)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not arg)) (widen))
        ((and (bound-and-true-p org-src-mode) (not arg)) (org-edit-src-exit))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         (cond ((ignore-errors (org-edit-src-code) t))
               ((ignore-errors (org-edit-latex-environment) t))
               ((ignore-errors (org-edit-latex-fragment) t))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;; This function should be bound to "SPC u" instead of
;; universal-argument to make "SPC u SPC u" work as expected.
(defun my/universal-argument (arg)
  "Begin a numeric argument, or multiply it by 4."
  (interactive "P")
  (if arg
      (universal-argument-more arg)
    (universal-argument)))

;; From https://github.com/bbatsov/crux.
(defun my/delete-file-and-buffer ()
  "Kill the current buffer and delete the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun my/split-window-vertically ()
  "Split window vertically, displaying other-buffer in a new
window, and switch to it."
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (switch-to-buffer (other-buffer)))

(defun my/split-window-horizontally ()
  "Split window horizontally, displaying other-buffer in a new
window, and switch to it."
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer (other-buffer)))

(defun my/switch-to-recent-buffer ()
  "Switch to the most recently visited buffer."
  (interactive)
  (switch-to-buffer nil))

(defun my/kill-buffer-and-window-or-layout ()
  "Kill the current buffer and delete its window or entire layout if
no other windows are present."
  (interactive)
  (if (> (count-windows) 1)
      (kill-buffer-and-window)
    (kill-buffer)
    (tab-bar-close-tab)))

;;; Global leader bindings

(leader-def
  "n" '(my/narrow-or-widen-dwim :which-key "narrow")
  "u" '(my/universal-argument :which-key "universal argument")
  "x" '(execute-extended-command :which-key "M-x")
  "X" '(execute-extended-command-for-buffer :which-key "M-X")
  "." '(repeat :which-key "repeat")
  "/" '(consult-line :which-key "search line")
  "SPC" '(vertico-repeat :which-key "repeat completion")

  "a" '(:ignore t :which-key "application")
  "ab" '(citar-open :which-key "bibliography")
  "ag" '(magit-status :which-key "git")
  "am" '(mu4e :which-key "mail")
  "ap" '(my/system-packages/body :which-key "system packages")
  "ar" '(elfeed :which-key "RSS")

  "b" '(:ignore t :which-key "buffer")
  "bb" '(my/switch-to-recent-buffer :which-key "most recent")
  "bc" '(clone-indirect-buffer :which-key "clone")
  "be" '(revert-buffer-with-coding-system :which-key "revert encoding")
  "bk" '(kill-current-buffer :which-key "kill")
  "bK" '(my/kill-buffer-and-window-or-layout :which-key "kill and close window")
  "bl" '(ibuffer :which-key "list")
  "bn" '(evil-buffer-new :which-key "new")
  "bo" '(:ignore t :which-key "other window")
  "boc" '(clone-indirect-buffer-other-window :which-key "clone")
  "bos" '(consult-buffer-other-window :which-key "switch")
  "br" '(revert-buffer-quick :which-key "revert")
  "bs" '(consult-buffer :which-key "switch")

  "c" '(:ignore t :which-key "credentials")
  "ce" '(password-store-edit :which-key "edit")
  "cf" '(password-store-copy-field :which-key "field")
  "cg" '(password-store-url :which-key "go to")
  "cl" '(my/password-store-copy-login :which-key "login")
  "cp" '(password-store-copy :which-key "password")

  "d" '(dictionary-search :which-key "dictionary")

  "f" '(:ignore t :which-key "file")
  "fa" '(find-alternate-file :which-key "find alternate")
  "fD" '(my/delete-file-and-buffer :which-key "delete")
  "ff" '(find-file :which-key "find")
  "fl" '(find-library :which-key "find library")
  "fo" '(:ignore t :which-key "other window")
  "foa" '(find-alternate-file-other-window :which-key "find alternate")
  "fof" '(find-file-other-window :which-key "find")
  "fol" '(find-library-other-window :which-key "find library")
  "fR" '(rename-visited-file :which-key "rename")
  "fr" '(consult-recent-file :which-key "recent")
  "fs" '(save-buffer :which-key "save")
  "fw" '(write-file :which-key "write")

  "g" '(:ignore t :which-key "go")
  "gb" '(consult-bookmark :which-key "bookmark")
  "gg" '(evil-avy-goto-char-timer :which-key "string")
  "gi" '(consult-imenu :which-key "imenu")
  "gj" '(evil-collection-consult-jump-list :which-key "jump list")
  "gl" '(link-hint-open-link :which-key "link")
  "gm" '(evil-collection-consult-mark :which-key "mark")
  "go" '(consult-outline :which-key "outline")
  "gw" '(evil-avy-goto-word-1 :which-key "word")

  "h" '(:keymap help-map :which-key "help")
  "hh" '(helpful-at-point :which-key "help at point")
  "hf" '(helpful-callable :which-key "function")
  "hk" '(helpful-key :which-key "key")
  "ho" '(helpful-symbol :which-key "symbol")
  "ht" '(tldr :which-key "tldr")
  "hv" '(helpful-variable :which-key "variable")

  "i" '(:ignore t :which-key "insert")
  "ik" '(consult-yank-pop :which-key "kill ring")
  "iu" '(insert-char :which-key "unicode")

  "l" '(:ignore t :which-key "layout")
  "ld" '(tab-bar-close-tab :which-key "delete")
  "lD" '(tab-bar-close-other-tabs :which-key "delete other")
  "lj" '(tab-bar-switch-to-next-tab :which-key "next")
  "lk" '(tab-bar-switch-to-prev-tab :which-key "previous")
  "ll" '(tab-bar-switch-to-recent-tab :which-key "most recent")
  "lL" '(tab-bar-move-tab :which-key "move right")
  "lH" '(tab-bar-move-tab-backward :which-key "move left")
  "ln" '(tab-bar-new-tab :which-key "new")
  "lr" '(tab-bar-rename-tab :which-key "rename")
  "ls" '(tab-bar-switch-to-tab :which-key "switch")
  "lu" '(tab-bar-undo-close-tab :which-key "undo delete")

  "o" '(:ignore t :which-key "org")
  "oa" '(org-agenda :which-key "agenda")
  "oc" '(org-capture :which-key "capture")
  "oi" '(org-mru-clock-in :which-key "clock in")
  "og" '(org-password-manager-generate-password :which-key "generate password")
  "ol" '(org-store-link :which-key "store link")
  "oo" '(org-clock-out :which-key "clock out")
  "op" '(org-password-manager-get-password :which-key "get password")
  "ou" '(org-password-manager-get-username :which-key "get username")

  "p" '(:ignore t :which-key "project")
  "pb" '(consult-project-buffer :which-key "switch to buffer")
  "pc" '(project-compile :which-key "compile")
  "pd" '(project-find-dir :which-key "find directory")
  "pf" '(project-find-file :which-key "find file")
  "pk" '(project-kill-buffers :which-key "kill")
  "ps" '(project-switch-project :which-key "switch")
  "p/" '(consult-ripgrep :which-key "search")
  "p!" '(project-shell-command :which-key "run shell command")
  "p&" '(project-async-shell-command :which-key "run async shell command")

  "q" '(:ignore t :which-key "quit")
  "qf" '(save-buffers-kill-terminal :which-key "close frame")
  "qq" '(save-buffers-kill-emacs :which-key "quit Emacs")

  "t" '(:ignore t :which-key "toggle")
  "tc" '(display-fill-column-indicator-mode :which-key "fill column")
  "tf" '(toggle-frame-fullscreen :which-key "fullscreen")
  "th" '(hl-line-mode :which-key "highlight line")
  "tl" '(toggle-tab-bar-mode-from-frame :which-key "layouts")
  "tm" '(toggle-frame-maximized :which-key "maximize")
  "tn" '(display-line-numbers-mode :which-key "line numbers")
  "to" '(olivetti-mode :which-key "olivetti")
  "tt" '(visual-line-mode :which-key "truncate lines")
  "tT" '(consult-theme :which-key "theme")
  "tv" '(variable-pitch-mode :which-key "variable pitch")
  "tw" '(whitespace-mode :which-key "whitespace")

  "w" '(:ignore t :which-key "window")
  "wd" '(delete-window :which-key "delete")
  "wD" '(delete-other-windows :which-key "delete other")
  "wh" '(my/split-window-horizontally :which-key "split horizontally")
  "ws" '(rotate-window :which-key "swap")
  "wt" '(rotate-layout :which-key "transpose")
  "wu" '(winner-undo :which-key "undo")
  "wr" '(winner-redo :which-key "redo")
  "wv" '(my/split-window-vertically :which-key "split vertically"))

;;; Global non-leader bindings

(general-define-key
 :states '(motion emacs normal)
 :keymaps 'override
 "M-o" #'other-window)

;; Rebind C-h to backspace globally.
(general-define-key
 :keymaps 'key-translation-map
 "C-h" "DEL")

;; OS-wide shortcuts.
(general-define-key
 :keymaps 'override
 "C-a" #'mark-whole-buffer
 "C-s" #'save-buffer
 (kbd "C-<next>") #'tab-bar-switch-to-next-tab
 (kbd "C-<prior>") #'tab-bar-switch-to-prev-tab)

(defun my/evil-paste-last-yanked ()
  "Paste last yanked text from register \"."
  (interactive)
  (evil-paste-from-register ?\"))

(general-define-key
 :states 'insert
 "C-n" #'completion-at-point
 "C-v" #'my/evil-paste-last-yanked)

(general-define-key
 :states '(motion normal)
 :keymaps 'override
 "C-j" #'evil-avy-goto-word-1)

;;; Evil collection

(use-package evil-collection
  :demand :after evil
  :init
  (setq evil-collection-setup-minibuffer t
        evil-collection-want-unimpaired-p nil)
  :config
  (setq evil-collection-key-blacklist
        (list leader-key local-leader-key))
  (evil-collection-init))
