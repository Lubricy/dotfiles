(after! terraform-mode
  (setq lsp-terraform-ls-enable-show-reference t)
  (setq lsp-terraform-ls-prefill-required-fields t)
  (add-hook! 'terraform-mode-hook #'lsp-deferred))
