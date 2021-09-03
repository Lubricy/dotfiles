;;; tools/lsp/config.el -*- lexical-binding: t; -*-

(defvar +lsp-company-backend 'company-lsp
  "What backend to prepend to `company-backends' when `lsp-mode' is active.

This can be a single company backend or a list thereof. It can be anything
`company-backends' will accept.")


;;
;;; Packages

(use-package! eglot
  :defer t)

 ;; cache candidates for better performance
