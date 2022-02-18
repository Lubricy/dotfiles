;; treemacs to show git directory highlights
(after! treemacs

  (setq treemacs-collapse-dirs 20
        +treemacs-git-mode 'deferred)
  ;; HACK
  (evil-define-key* 'treemacs treemacs-mode-map (kbd "h") nil))
