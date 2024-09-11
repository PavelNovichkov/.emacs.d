;;; core-ui.el --- User interface setup -*- lexical-binding: t; -*-

;;; Cursor

(blink-cursor-mode -1)

;;; Modeline

(use-package nano-modeline
  :hook
  (prog-mode . nano-modeline-prog-mode)
  (text-mode . nano-modeline-text-mode)
  (org-mode . nano-modeline-org-mode)
  (pdf-view-mode . nano-modeline-pdf-mode)
  (mu4e-headers-mode . nano-modeline-mu4e-headers-mode)
  (mu4e-view-mode . nano-modeline-mu4e-message-mode)
  (elfeed-show-mode . nano-modeline-elfeed-entry-mode)
  (elfeed-search-mode . nano-modeline-elfeed-search-mode)
  (term-mode . nano-modeline-term-mode)
  (xwidget-webkit-mode . nano-modeline-xwidget-mode)
  (messages-buffer-mode . nano-modeline-message-mode)
  (org-capture-mode . nano-modeline-org-capture-mode)
  (org-agenda-mode . nano-modeline-org-agenda-mode)
  :custom
  (mode-line-format nil)
  :config
  (nano-modeline-text-mode t)
  ;; HACK Update nano-modeline active face, see
  ;; https://github.com/rougier/nano-modeline/issues/63.
  (defun my/nano-modeline-update (&rest _)
    "Update nano-modeline active face."
    (custom-set-faces
     `(nano-modeline-active
       ((t (:foreground ,(face-foreground 'default)
            :background ,(face-background 'header-line nil t)
            :box (:line-width 1 :color ,(face-background 'default))))))))
  (add-hook 'enable-theme-functions #'my/nano-modeline-update))

;;; Posframe

(use-package posframe
  :demand)

;;; All the icons

(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1.0))

(use-package all-the-icons-completion
  :demand
  :custom
  (all-the-icons-completion-mode t))

;;; Window management

(defun my/suppress-delete-other-windows (fn &rest args)
  "Ignore `delete-other-windows' to prevent deleting windows."
  (cl-letf (((symbol-function 'delete-other-windows) 'ignore))
    (apply fn args)))
