;;; emacs/outline.el -*- lexical-binding: t; -*-

(use-package outline ; built-in
  :config
  ;; Customize the folding marker to be " +" instead of "...". For details, see
  ;; https://www.reddit.com/r/emacs/comments/e2u5n9/code_folding_with_outlineminormode/
  (set-display-table-slot
   standard-display-table
   'selective-display
   (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
     (vconcat (mapcar (lambda (c) (+ face-offset c)) " +")))))

(use-package outline-magic
  :demand :after outline
  :config
  (setq outline-cycle-emulate-tab t)
  (general-define-key
   :states 'normal
   :keymaps 'outline-minor-mode-map
   "TAB" #'outline-cycle
   "<tab>" #'outline-cycle))
