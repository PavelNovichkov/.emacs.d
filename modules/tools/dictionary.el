;;; tools/dictionary.el -*- lexical-binding: t; -*-

(use-package google-translate-smooth-ui
  :straight google-translate
  :commands google-translate-smooth-translate
  :config
  (setq google-translate-backend-method 'curl) ;; built-in method causes errors
  (setq google-translate-translation-directions-alist
        '(("en" . "ru") ("ru" . "en") ("it" . "en") ("en" . "it")))
  ;; Default keybindings are lost due to evil minibuffer setup, so add
  ;; them explicitly.
  (google-translate--setup-minibuffer-keymap)
  (general-define-key
   :states 'insert
   :keymaps 'google-translate-minibuffer-keymap
   "C-p" #'google-translate-previous-translation-direction
   "C-n" #'google-translate-next-translation-direction))

(use-package powerthesaurus
  :commands powerthesaurus-lookup-word)

(use-package lexic
  :straight (:host github :repo "tecosaur/lexic")
  :ensure-system-package sdcv)
