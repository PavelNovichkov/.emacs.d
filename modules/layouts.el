;;; layouts.el -*- lexical-binding: t; -*-

(use-package tab-bar ; built-in
  :straight nil
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-show nil)
  :init
  (setq tab-bar-new-tab-choice "*scratch*"
        tab-bar-new-tab-to 'rightmost)
  (defun my/tab-bar-toggle ()
    "Toggle the visibility of the tab bar."
    (interactive)
    (if tab-bar-show
        (customize-set-variable 'tab-bar-show nil)
      (customize-set-variable 'tab-bar-show t)))
  ;; Override the workspace modeline segment to always show the tab name.
  (defun my/doom-modeline-segment--workspace-name ()
    (when (and doom-modeline-workspace-name (featurep 'tab-bar))
      (when-let
          ((name (let* ((current-tab (tab-bar--current-tab))
                        (tab-index (tab-bar--current-tab-index))
                        (explicit-name (alist-get 'explicit-name current-tab))
                        (tab-name (alist-get 'name current-tab)))
                   (if explicit-name tab-name (+ 1 tab-index)))))
        (propertize (format " %s " name) 'face
                    (if (doom-modeline--active)
                        'doom-modeline-buffer-major-mode
                      'mode-line-inactive)))))
  (with-eval-after-load 'doom-modeline
    (advice-add 'doom-modeline-segment--workspace-name
                :override #'my/doom-modeline-segment--workspace-name)))

(use-package rotate
  :commands (rotate-window rotate-layout))
