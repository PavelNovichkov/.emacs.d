;;; app/system-packages.el -*- lexical-binding: t; -*-

(use-package system-packages
  :commands my/system-packages/body
  :init
  (defhydra my/system-packages (:color teal :columns 2)
    "Manage system packages"
    ("u" system-packages-update "update")
    ("i" system-packages-install "install")
    ("U" system-packages-uninstall "uninstall")
    ("O" system-packages-remove-orphaned "remove orphans")
    ("C" system-packages-clean-cache "clean cache")
    ("s" system-packages-search "search")
    ("d" system-packages-get-info "describe")
    ("ll" system-packages-log "log")
    ("li" system-packages-list-installed-packages "list installed")
    ("lf" system-packages-list-files-provided-by "list files")
    ("f" system-packages-owning-file "file owner")
    ("vf" system-packages-verify-all-packages "verify files")
    ("vd" system-packages-verify-all-dependencies "verify dependencies")
    ("ld" system-packages-list-dependencies-of "list dependencies")))
