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
  "n" '("narrow" . my/narrow-or-widen-dwim)
  "u" '("universal argument" . my/universal-argument)
  "x" '("M-x" . execute-extended-command)
  "X" '("M-X" . execute-extended-command-for-buffer)
  "." '("repeat" . repeat)
  "/" '("search line" . consult-line)
  "SPC" '("repeat completion" . vertico-repeat)

  "a" '("application" . (keymap))
  "ab" '("bibliography" . citar-open)
  "ag" '("git" . magit-status)
  "am" '("mail" . mu4e)
  "ap" '("system packages" . my/system-packages/body)
  "ar" '("RSS" . elfeed)
  "at" '("top" . proced)

  "b" '("buffer" . (keymap))
  "bb" '("most recent" . my/switch-to-recent-buffer)
  "bc" '("clone" . clone-indirect-buffer)
  "be" '("revert encoding" . revert-buffer-with-coding-system)
  "bk" '("kill" . kill-current-buffer)
  "bK" '("kill and close window" . my/kill-buffer-and-window-or-layout)
  "bl" '("list" . ibuffer)
  "bn" '("new" . evil-buffer-new)
  "bo" '("other window" . (keymap))
  "boc" '("clone" . clone-indirect-buffer-other-window)
  "bos" '("switch" . consult-buffer-other-window)
  "br" '("revert" . revert-buffer-quick)
  "bs" '("switch" . consult-buffer)

  "c" '("credentials" . (keymap))
  "ce" '("edit" . password-store-edit)
  "cf" '("field" . password-store-copy-field)
  "cg" '("go to" . password-store-url)
  "cl" '("login" . my/password-store-copy-login)
  "cp" '("password" . password-store-copy)

  "d" '("dictionary" . dictionary-search)

  "f" '("file" . (keymap))
  "fa" '("find alternate" . find-alternate-file)
  "fD" '("delete" . my/delete-file-and-buffer)
  "ff" '("find" . find-file)
  "fl" '("find library" . find-library)
  "fo" '("other window" . (keymap))
  "foa" '("find alternate" . find-alternate-file-other-window)
  "fof" '("find" . find-file-other-window)
  "fol" '("find library" . find-library-other-window)
  "fR" '("rename" . rename-visited-file)
  "fr" '("recent" . consult-recent-file)
  "fs" '("save" . save-buffer)
  "fw" '("write" . write-file)

  "g" '("go" . (keymap))
  "gb" '("bookmark" . consult-bookmark)
  "gg" '("string" . evil-avy-goto-char-timer)
  "gi" '("imenu" . consult-imenu)
  "gj" '("jump list" . evil-collection-consult-jump-list)
  "gl" '("link" . link-hint-open-link)
  "gm" '("mark" . evil-collection-consult-mark)
  "go" '("outline" . consult-outline)

  "h" (cons "help" help-map)
  "hh" '("help at point" . helpful-at-point)
  "hf" '("function" . helpful-callable)
  "hk" '("key" . helpful-key)
  "ho" '("symbol" . helpful-symbol)
  "ht" '("tldr" . tldr)
  "hv" '("variable" . helpful-variable)

  "i" '("insert" . (keymap))
  "ik" '("kill ring" . consult-yank-pop)
  "iu" '("unicode" . insert-char)

  "l" '("layout" . (keymap))
  "ld" '("delete" . tab-bar-close-tab)
  "lD" '("delete other" . tab-bar-close-other-tabs)
  "lj" '("next" . tab-bar-switch-to-next-tab)
  "lk" '("previous" . tab-bar-switch-to-prev-tab)
  "ll" '("most recent" . tab-bar-switch-to-recent-tab)
  "lL" '("move right" . tab-bar-move-tab)
  "lH" '("move left" . tab-bar-move-tab-backward)
  "ln" '("new" . tab-bar-new-tab)
  "lr" '("rename" . tab-bar-rename-tab)
  "ls" '("switch" . tab-bar-switch-to-tab)
  "lu" '("undo delete" . tab-bar-undo-close-tab)

  "o" '("org" . (keymap))
  "oa" '("agenda" . org-agenda)
  "oc" '("capture" . org-capture)
  "oi" '("clock in" . org-mru-clock-in)
  "og" '("generate password" . org-password-manager-generate-password)
  "ol" '("store link" . org-store-link)
  "oo" '("clock out" . org-clock-out)
  "op" '("get password" . org-password-manager-get-password)
  "ou" '("get username" . org-password-manager-get-username)

  "p" '("project" . (keymap))
  "pb" '("switch to buffer" . consult-project-buffer)
  "pc" '("compile" . project-compile)
  "pd" '("find directory" . project-find-dir)
  "pf" '("find file" . project-find-file)
  "pk" '("kill" . project-kill-buffers)
  "ps" '("switch" . project-switch-project)
  "p." '("root directory" . project-dired)
  "p/" '("search" . consult-ripgrep)
  "p!" '("run shell command" . project-shell-command)
  "p&" '("run async shell command" . project-async-shell-command)

  "q" '("quit" . (keymap))
  "qf" '("close frame" . save-buffers-kill-terminal)
  "qq" '("quit Emacs" . save-buffers-kill-emacs)
  "qr" '("restart Emacs" . restart-emacs)

  "s" '("snipe" . evil-avy-goto-char-2)

  "t" '("toggle" . (keymap))
  "tc" '("fill column" . display-fill-column-indicator-mode)
  "tf" '("fullscreen" . toggle-frame-fullscreen)
  "th" '("highlight line" . hl-line-mode)
  "tl" '("layouts" . toggle-tab-bar-mode-from-frame)
  "tm" '("maximize" . toggle-frame-maximized)
  "tn" '("line numbers" . display-line-numbers-mode)
  "to" '("olivetti" . olivetti-mode)
  "tt" '("truncate lines" . visual-line-mode)
  "tT" '("theme" . consult-theme)
  "tv" '("variable pitch" . variable-pitch-mode)
  "tw" '("whitespace" . whitespace-mode)

  "w" '("window" . (keymap))
  "wd" '("delete" . delete-window)
  "wD" '("delete other" . delete-other-windows)
  "wh" '("split horizontally" . my/split-window-horizontally)
  "wo" '("toggle side windows" . window-toggle-side-windows)
  "wp" '("pin" . my/window-toggle-dedicated)
  "ws" '("swap" . rotate-window)
  "wt" '("transpose" . rotate-layout)
  "wu" '("undo" . winner-undo)
  "wr" '("redo" . winner-redo)
  "wv" '("split vertically" . my/split-window-vertically))

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
 "C-SPC" #'sp-up-sexp ; Consistent with Mathematica
 "C-n" #'completion-at-point
 "C-t" #'cape-dabbrev
 "C-v" #'my/evil-paste-last-yanked)

(general-define-key
 :states '(motion normal)
 :keymaps 'override
 "C-j" #'evil-avy-goto-word-1)
