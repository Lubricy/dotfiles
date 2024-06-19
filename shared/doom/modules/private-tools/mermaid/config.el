(use-package! mermaid-mode
  :mode "\\.mermaid\\'"
  :mode "\\.mmd\\'"
  :init
  (defcustom mermaid-executable "mmdc"
    "executable path for mermaid cli")
  :config
  (setq mermaid-mmdc-location (executable-find mermaid-executable))
  (setq ob-mermaid-cli-path (executable-find mermaid-executable)))
