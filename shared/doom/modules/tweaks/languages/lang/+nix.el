(after! projectile
  (add-to-list 'projectile-project-root-files "flake.nix")
  (add-to-list 'projectile-project-root-files "README.md"))

(after! nix
  (setq! lsp-nix-nil-formatter ["nixpkgs-fmt"])
  (add-hook! 'nix-mode-hook #'lsp-deferred))
