(use-package! docstr
  :defer 1
  :config
  (docstr-faces-apply)
  (global-docstr-mode 1)
  (setq! docstr-python-style 'numpy))
