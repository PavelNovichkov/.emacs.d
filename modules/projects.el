;;; projects.el -*- lexical-binding: t; -*-

(use-package projectile
  :demand
  :config
  (setq projectile-project-search-path '("~/Documents/git/")
        projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'alien)
  (when (executable-find "fd")
    (setq projectile-git-command "fd -H -t f -0 -E .git"
          projectile-generic-command "fd -H -t f -0 --ignore-file .projectile")))

(use-package counsel-projectile
  :after projectile
  :init
  (setq counsel-projectile-mode-map nil)
  :config
  (counsel-projectile-mode))
