;;; tools/pass.el -*- lexical-binding: t; -*-

(use-package password-store
  :commands my/password-store-copy-login
  :init
  (setq password-store-executable (executable-find "gopass"))
  (auth-source-pass-enable)
  (setq auth-source-do-cache nil)
  :config
  (defun my/password-store-copy-login (entry)
  "Add login (username) for ENTRY into the kill ring."
  (interactive (list (password-store--completing-read t)))
  (let* ((field "username")
         (value (password-store-get-field entry field)))
    (password-store--save-field-in-kill-ring entry value field))))
