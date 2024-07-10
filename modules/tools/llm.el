;;; tools/llm.el -*- lexical-binding: t; -*-

(use-package ellama
  :config
  (require 'llm-ollama)

  (setopt
   ellama-provider
   (make-llm-ollama
    :chat-model "gemma2"
    :embedding-model "nomic-embed-text")
   ellama-sessions-directory
   (file-name-concat no-littering-var-directory "ellama-sessions")
   ellama-fill-paragraphs nil))
