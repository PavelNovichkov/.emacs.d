;;; ui/hl-todo.el -*- lexical-binding: t; -*-

(use-package hl-todo
  :hook ((prog-mode yaml-mode) . hl-todo-mode))
