;; load the stock packages.el from ~/.emacs.d/modules/lang/lean/
(load-file (expand-file-name "modules/lang/lean/packages.el" doom-emacs-dir))

(when (modulep! :lang lean +lean4)
  (package! lean4-mode
    :recipe (:host github
             :repo "leanprover-community/lean4-mode"
             :files ("*.el" "data"))))
