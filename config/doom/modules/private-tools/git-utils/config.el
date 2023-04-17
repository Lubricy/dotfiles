(use-package blamer
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :after git
  :custom-face
  (blamer-face ((t :foreground "#6977be"
                    :background nil
                    :height 100
                    :italic t)))
  :config
  (setq blamer-self-author-name "You")
  (add-hook! prog-mode #'blamer-mode))

(use-package conventional-commit
  :after git
  :hook (git-commit-mode . conventional-commit-setup))
