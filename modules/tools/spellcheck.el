;;; tools/spellcheck.el -*- lexical-binding: t; -*-

(use-package ispell ; built-in
  :ensure-system-package hunspell
  :straight nil
  :config
  ;; Advice to inhibit the minibuffer message.
  (defun my/ispell-quiet-init (orig-fun &rest args)
    "Inhibit \"Starting new Ispell process\" message in the minibuffer."
    (let ((inhibit-message t))
      (apply orig-fun args)))
  (advice-add #'ispell-init-process :around #'my/ispell-quiet-init)
  (setq ispell-program-name "hunspell")
  (ispell-set-spellchecker-params)
  ;; Use both english and russian dictionaries at once.
  (ispell-hunspell-add-multi-dic "en_US,ru_RU")
  (setq ispell-dictionary "en_US,ru_RU"))

(use-package flyspell ; built-in
  :straight nil
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  (defun my/straighten-underline (face)
    "Change underline style of FACE from wave to straight line."
    (let ((underline (face-attribute face :underline)))
      (when (eq (plist-get underline :style) 'wave)
        (plist-put underline :style 'line)
        (set-face-attribute face nil :underline underline))))
  (seq-do 'my/straighten-underline '(flyspell-duplicate flyspell-incorrect)))

(use-package flyspell-correct
  :commands (flyspell-correct-previous)
  :init
  (general-define-key :states 'normal "z=" #'flyspell-correct-previous))

(use-package flyspell-correct-ivy
  :demand :after flyspell-correct)

;; TODO: add spellcheck hydra.
