;;; lang/markdown.el -*- lexical-binding: t; -*-

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)

  :custom
  (markdown-enable-highlighting-syntax t)
  (markdown-enable-math t)
  (markdown-fontify-code-blocks-natively t)

  :config
  (local-leader-def
    :keymaps 'markdown-mode-map
    "l" '(markdown-insert-link :which-key "insert link")
    "t" '(:ignore t :which-key "toggle")
    "ti" '(markdown-toggle-inline-images :which-key "images")
    "tl" '(markdown-toggle-url-hiding :which-key "links")
    "tm" '(markdown-toggle-markup-hiding :which-key "markup")))
