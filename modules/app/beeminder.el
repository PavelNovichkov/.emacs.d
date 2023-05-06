;;; app/beeminder.el -*- lexical-binding: t; -*-

(use-package beeminder
  :init
  (with-eval-after-load 'org
    (remove-hook 'org-after-todo-state-change-hook
                 #'beeminder--on-org-task-completed)))
