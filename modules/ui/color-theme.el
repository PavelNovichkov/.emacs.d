;;; ui/color-theme.el -*- lexical-binding: t; -*-

(defun my/add-padding (face size)
  `(,face ((t :box (:line-width ,size :color ,(face-background face))))))

(defun my/customize-faces (&rest _)
  "Customize faces."
  (let ((bg (face-background 'default)))
    (custom-set-faces
     ;; Add padding to tabs
     (my/add-padding 'tab-bar-tab 6)
     (my/add-padding 'tab-bar-tab-inactive 6)
     ;; Invisible borders
     `(fringe ((t :background ,bg :foreground ,bg)))
     `(window-divider ((t :background ,bg :foreground ,bg)))
     `(window-divider-first-pixel ((t :background ,bg :foreground ,bg)))
     `(window-divider-last-pixel ((t :background ,bg :foreground ,bg))))))
(add-hook 'enable-theme-functions #'my/customize-faces)


(use-package modus-themes
  :demand

  :init
  (defun my/customize-modus-themes (theme)
    "Customize Modus themes."
    (when (memq theme modus-themes-items)
      (modus-themes-with-colors
        (custom-set-faces
         '(bold ((t :weight unspecified)))
         `(calendar-today ((t :underline nil :foreground ,date-scheduled)))
         '(modus-themes-button ((t :box nil))) ; flat buttons
         '(mu4e-header-face ((t :inherit default)))
         '(org-table ((t :inherit unspecified)))))))
  (add-hook 'enable-theme-functions #'my/customize-modus-themes)

  :config
  (require-theme 'modus-themes)

  (setopt
   modus-themes-headings '((t . (1.2)))
   modus-themes-mixed-fonts nil)

  (setopt modus-vivendi-palette-overrides
   '((bg-main "#1e1e1e")
     (bg-dim "#303030")
     (bg-inactive "#404040")
     (bg-region "#2f3849")))

  (setopt
   modus-themes-common-palette-user
   '((my/string green-intense)
     (my/constant magenta-faint)
     (my/comment red-cooler)
     (my/definition blue-warmer)
     (my/highlight bg-dim)
     (my/link blue-warmer)
     (my/match fg-main)))

  (setopt
   modus-operandi-palette-user
   '((my/select bg-cyan-nuanced)))

  (setopt
   modus-vivendi-palette-user
   '((my/select bg-cyan-subtle)))

  (setopt
   modus-themes-common-palette-overrides
   '(

;;; Special purpose

     (bg-completion my/select)
     (bg-hover my/highlight)
     (bg-hl-line my/highlight)
     (bg-region bg-blue-subtle)

     (bg-tab-bar bg-main)
     (bg-tab-current my/highlight)
     (bg-tab-other bg-main)

;;; Diffs

     (fg-added unspecified)
     (fg-added-intense unspecified)
     (fg-changed unspecified)
     (fg-changed-intense unspecified)
     (fg-removed unspecified)
     (fg-removed-intense unspecified)

;;; Mappings

;;;; General mappings

     (fringe bg-main)
     (keybind my/link)

;;;; Code mappings (inspired by the Alabaster theme)

     ; strings
     (string my/string)
     (docstring my/string)
     (docmarkup my/string)
     ; static constants
     (builtin my/constant)
     (constant my/constant)
     ; comments
     (comment my/comment)
     ; global definitions
     (fnname my/definition)
     (variable my/definition)
     ; rest
     (keyword fg-main)
     (preprocessor fg-main)
     (type fg-main)
     (rx-construct my/constant)
     (rx-backslash my/constant)

;;;; Completion mappings

     (fg-completion-match-0 my/match)
     (fg-completion-match-1 my/match)
     (fg-completion-match-2 my/match)
     (fg-completion-match-3 my/match)

;;;; Line number mappings

     (bg-line-number-inactive bg-main)
     (bg-line-number-active my/highlight)

;;;; Link mappings

     (fg-link my/link)
     (underline-link unspecified)

     (fg-link-symbolic my/link)
     (underline-link-symbolic unspecified)

     (fg-link-visited my/link)
     (underline-link-visited unspecified)

;;;; Mark mappings

     ;; Change only background.
     (fg-mark-delete unspecified)
     (fg-mark-select unspecified)
     (fg-mark-other unspecified)

;;;; Heading mappings

     (fg-heading-1 fg-main)
     (fg-heading-2 fg-main)
     (fg-heading-3 fg-main)
     (fg-heading-4 fg-main)
     (fg-heading-5 fg-main)
     (fg-heading-6 fg-main)
     (fg-heading-7 fg-main)
     (fg-heading-8 fg-main)))

  ;; Load both themes, otherwise auto-dark does not apply some overrides correctly.
  (modus-themes-load-theme 'modus-vivendi)
  (modus-themes-load-theme 'modus-operandi))


(use-package auto-dark
  :demand :after modus-themes

  :config
  (setopt
   auto-dark-themes '((modus-vivendi) (modus-operandi)))

  (defun my/pdf-tools-set-colors ()
    "Set colors in current PDFView buffer based on the color theme."
    (let ((arg (if (auto-dark--is-dark-mode) 1 -1)))
      (pdf-view-midnight-minor-mode arg)))

  (add-hook 'pdf-view-mode-hook #'my/pdf-tools-set-colors)

  (defun my/pdf-tools-update-colors-in-buffers (&rest _)
    "Update colors in all PDFView buffers based on the color theme."
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode 'pdf-view-mode)
          (my/pdf-tools-set-colors)))))

  (add-hook 'enable-theme-functions #'my/pdf-tools-update-colors-in-buffers)

  (auto-dark-mode))
