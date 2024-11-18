(after! projectile
  (setq projectile-project-search-path '("~/Projects/")
        projectile-create-missing-test-files t
        projectile-run-use-comint-mode t)
  (add-to-list 'projectile-globally-ignored-files ".direnv"))

(dolist (flag (doom-module :tweaks 'projectile :flags))
  (load! (symbol-name flag)))
