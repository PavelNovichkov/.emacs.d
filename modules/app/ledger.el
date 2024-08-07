;;; app/ledger.el -*- lexical-binding: t; -*-

(use-package ledger-mode
  :ensure-system-package ledger
  :commands ledger-mode
  :hook (ledger-mode . ledger-flymake-enable)
  :config
  (setq ledger-reports
        '(("bal"            "%(binary) -f %(ledger-file) bal")
          ("bal this month" "%(binary) -f %(ledger-file) bal -p %(month) -S amount")
          ("bal this year"  "%(binary) -f %(ledger-file) bal -p 'this year'")
          ("net worth"      "%(binary) -f %(ledger-file) bal Assets Liabilities")
          ("account"        "%(binary) -f %(ledger-file) reg %(account)"))))

(use-package evil-ledger
  :demand :after ledger-mode
  :hook (ledger-mode . evil-ledger-mode))
