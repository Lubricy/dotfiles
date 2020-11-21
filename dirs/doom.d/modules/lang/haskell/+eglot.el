;;; lang/haskell/+eglot.el -*- lexical-binding: t; -*-
;;;###if (featurep! +eglot)

(use-package! eglot
  :after haskell-mode
  :init (add-hook! 'haskell-mode-hook #'eglot-ensure))
  ;; :config
  ;; (add-to-list 'eglot-server-programs '(haskell-mode . ("ghcide" "--lsp"))))
