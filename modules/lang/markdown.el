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
    "l" '("insert link" . markdown-insert-link)
    "t" '("toggle" . (keymap))
    "ti" '("images" . markdown-toggle-inline-images)
    "tl" '("links" . markdown-toggle-url-hiding)
    "tm" '("markup" . markdown-toggle-markup-hiding)))
