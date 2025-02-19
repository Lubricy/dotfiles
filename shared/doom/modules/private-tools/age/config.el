(use-package! age
  :config
  (age-file-enable)
  :custom
  (age-default-identity "~/.ssh/id_ed25519")
  (age-default-recipient (+age-github-keys-for "Lubricy")))
