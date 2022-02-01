(use-package! hydra-posframe
  :after hydra
  :config
  (setq hydra-posframe-poshandler 'posframe-poshandler-frame-bottom-right-corner)
  (hydra-posframe-mode))
