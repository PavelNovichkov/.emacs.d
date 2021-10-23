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

;; From https://github.com/bbatsov/crux.
(defun my/rename-file-and-buffer ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((new-name (read-from-minibuffer "New name: " filename))
             (containing-dir (file-name-directory new-name)))
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

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

(defun my/tab-bar-move-tab-left ()
  "Move current tab to the left."
  (interactive)
  (tab-bar-move-tab -1))

;;; Global leader bindings

(leader-def
  "n" '(my/narrow-or-widen-dwim :which-key "narrow")
  "u" '(my/universal-argument :which-key "universal argument")
  "x" '(counsel-M-x :which-key "M-x")
  "." '(repeat :which-key "repeat")
  "/" '(swiper :which-key "swiper")
  "?" '(swiper-isearch :which-key "swiper")
  "SPC" '(ivy-resume :which-key "repeat ivy query")

  "a" '(:ignore t :which-key "application")
  "ab" '(ivy-bibtex :which-key "bibliography")
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
  "bos" '(ivy-switch-buffer-other-window :which-key "switch")
  "br" '(revert-buffer :which-key "revert")
  "bs" '(ivy-switch-buffer :which-key "switch")

  "d" '(:ignore t :which-key "dictionary")
  "dd" '(lexic-search :which-key "meaning")
  "ds" '(powerthesaurus-lookup-word :which-key "synonyms")
  "dt" '(google-translate-smooth-translate :which-key "translate")

  "f" '(:ignore t :which-key "file")
  "fa" '(find-alternate-file :which-key "find alternate")
  "fD" '(my/delete-file-and-buffer :which-key "delete")
  "ff" '(counsel-find-file :which-key "find")
  "fl" '(counsel-find-library :which-key "find library")
  "fo" '(:ignore t :which-key "other window")
  "foa" '(find-alternate-file-other-window :which-key "find alternate")
  "fof" '(find-file-other-window :which-key "find")
  "fol" '(find-library-other-window :which-key "find library")
  "fR" '(my/rename-file-and-buffer :which-key "rename")
  "fr" '(counsel-recentf :which-key "recent")
  "fs" '(save-buffer :which-key "save")
  "fw" '(write-file :which-key "write")
  "f/" '(counsel-fzf :which-key "search")

  "g" '(:ignore t :which-key "go")
  "gb" '(counsel-bookmark :which-key "bookmark")
  "gg" '(evil-avy-goto-char-timer :which-key "string")
  "gi" '(counsel-imenu :which-key "imenu")
  "gl" '(link-hint-open-link :which-key "link")
  "go" '(counsel-outline :which-key "outline")
  "gw" '(evil-avy-goto-word-1 :which-key "word")

  "h" '(:keymap help-map :which-key "help")
  "hh" '(helpful-at-point :which-key "help at point")
  "hf" '(helpful-callable :which-key "function")
  "hk" '(helpful-key :which-key "key")
  "ho" '(helpful-symbol :which-key "symbol")
  "ht" '(tldr :which-key "tldr")
  "hv" '(helpful-variable :which-key "variable")

  "i" '(:ignore t :which-key "insert")
  ;; This option does not show which-key descriptions correctly:
  ;; "ic" '(:keymap iso-transl-ctl-x-8-map :package iso-transl :which-key "compose")
  "ic" `(,(general-simulate-key "C-x 8") :which-key "compose")
  "ik" '(counsel-yank-pop :which-key "kill ring")
  "io" '(counsel-org-entity :which-key "org entity")
  "ir" '(counsel-evil-registers :which-key "registers")
  "iu" '(counsel-unicode-char :which-key "unicode")

  "l" '(:ignore t :which-key "layout")
  "ld" '(tab-bar-close-tab :which-key "delete")
  "lD" '(tab-bar-close-other-tabs :which-key "delete other")
  "lj" '(tab-bar-switch-to-next-tab :which-key "next")
  "lk" '(tab-bar-switch-to-prev-tab :which-key "previous")
  "ll" '(tab-bar-switch-to-recent-tab :which-key "most recent")
  "lL" '(tab-bar-move-tab :which-key "move right")
  "lH" '(my/tab-bar-move-tab-left :which-key "move left")
  "ln" '(tab-bar-new-tab :which-key "new")
  "lr" '(tab-bar-rename-tab :which-key "rename")
  "ls" '(tab-bar-switch-to-tab :which-key "switch")
  "lu" '(tab-bar-undo-close-tab :which-key "undo delete")

  "o" '(:ignore t :which-key "org")
  "oa" '(org-agenda :which-key "agenda")
  "oc" '(org-capture :which-key "capture")
  "og" '(org-password-manager-generate-password :which-key "generate password")
  "oh" '(counsel-org-agenda-headlines :which-key "go to headline")
  "ol" '(org-store-link :which-key "store link")
  "op" '(org-password-manager-get-password :which-key "get password")
  "ot" '(org-mru-clock-in :which-key "timer")
  "ou" '(org-password-manager-get-username :which-key "get username")

  "p" '(:ignore t :which-key "project")
  "pb" '(projectile-ibuffer :which-key "list buffers")
  "pc" '(projectile-compile-project :which-key "compile")
  "pd" '(counsel-projectile-find-dir :which-key "find directory")
  "pf" '(counsel-projectile-find-file :which-key "find file")
  "pI" '(projectile-invalidate-cache :which-key "invalidate cache")
  "pk" '(projectile-kill-buffers :which-key "kill")
  "ps" '(counsel-projectile-switch-project :which-key "switch")
  "p/" '(counsel-projectile-ag :which-key "search")
  "p!" '(projectile-run-shell-command-in-root :which-key "run shell command")
  "p&" '(projectile-run-async-shell-command-in-root :which-key "run async shell command")

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
  "tt" '(toggle-truncate-lines :which-key "truncate lines")
  "tT" '(counsel-load-theme :which-key "theme")
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

(defun my/evil-paste-last-yanked ()
  "Paste last yanked text from register 0."
  (interactive)
  (evil-paste-from-register ?0))

(general-define-key
 :states 'insert
 "C-n" #'company-complete
 "C-v" #'my/evil-paste-last-yanked)

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

;; Setup ivy for evil states in minibuffer.
(general-define-key
 :states 'insert
 :keymaps 'ivy-minibuffer-map
 "DEL" #'ivy-backward-delete-char
 "C-r" #'evil-paste-from-register
 "C-o" #'hydra-ivy/body)

(general-define-key
 :states 'normal
 :keymaps 'ivy-minibuffer-map
 "/" #'ivy-reverse-i-search
 "?" #'ivy-reverse-i-search)
