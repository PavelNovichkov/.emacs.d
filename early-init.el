;; Redirect native compilation cache.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;; Set some UI variables.
(setq frame-title-format "Emacs")
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq-default mode-line-format nil)

(modify-all-frames-parameters
 '((fullscreen . maximized)
   (internal-border-width . 12)
   (right-divider-width . 15)
   (bottom-divider-width . 15)))

(setq package-enable-at-startup nil)
