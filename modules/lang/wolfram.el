;;; lang/wolfram.el -*- lexical-binding: t; -*-

(use-package wolfram-mode
  :mode "\\.\\(wl\\|wls\\|m\\)\\'"
  :hook (wolfram-mode . my/setup-wolfram-mode)
  :config
  (defun my/setup-wolfram-mode ()
    "Wolfram-mode-hook setup."
    (setq-local fill-column 80))
  (setq wolfram-indent 2)
  (with-eval-after-load 'eglot
    (let ((mode '(wolfram-mode :language-id "Wolfram Language"))
          (contact '("WolframKernel"
                     "-noinit"
                     "-noprompt"
                     "-nopaclet"
                     "-noicon"
                     "-nostartuppaclets"
                     "-run"
                     "Needs[\"LSPServer`\"];LSPServer`StartServer[]")))
      (add-to-list 'eglot-server-programs (cons mode contact)))))
