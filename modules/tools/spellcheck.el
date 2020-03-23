;;; tools/spellcheck.el -*- lexical-binding: t; -*-

(use-package ispell ; built-in
  :straight nil
  :config
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

;; TODO: add flyspell-correct-ivy.
