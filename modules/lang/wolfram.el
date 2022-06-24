;;; lang/wolfram.el -*- lexical-binding: t; -*-

(use-package wolfram-mode
  :mode "\\.\\(wl\\|wls\\|m\\)\\'"
  :config
  (setq wolfram-indent 4)
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
