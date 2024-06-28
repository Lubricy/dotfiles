(after! nix
  (setq! lsp-nix-nil-formatter ["nixpkgs-fmt"])
  (add-hook! 'nix-mode-hook #'lsp-deferred))
