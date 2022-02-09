(use-package blamer
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#6977be"
                    :background nil
                    :height 100
                    :italic t)))
  :config
  (setq blamer-self-author-name "You")
  (add-hook! prog-mode #'blamer-mode))
