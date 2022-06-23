;;; emacs/outline.el -*- lexical-binding: t; -*-

(use-package outline ; built-in
  :straight nil
  :config
  (setq outline-minor-mode-cycle t)
  ;; Customize the folding marker to be " +" instead of "...". For details, see
  ;; https://www.reddit.com/r/emacs/comments/e2u5n9/code_folding_with_outlineminormode/
  (set-display-table-slot
   standard-display-table
   'selective-display
   (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
     (vconcat (mapcar (lambda (c) (+ face-offset c)) " +")))))
