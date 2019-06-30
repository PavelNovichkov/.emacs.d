;;; tools/spellcheck.el -*- lexical-binding: t; -*-

(use-package ispell ; built-in
  :config
  (setq ispell-program-name "hunspell")
  (ispell-set-spellchecker-params)
  ;; Use both english and russian dictionaries at once.
  (ispell-hunspell-add-multi-dic "en_US,ru_RU")
  (setq ispell-dictionary "en_US,ru_RU"))

(use-package flyspell ; built-in
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

;; TODO: add flyspell-correct-ivy.
