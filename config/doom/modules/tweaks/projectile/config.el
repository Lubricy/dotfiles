(after! projectile
  (setq projectile-project-search-path '("~/Projects/")
        projectile-create-missing-test-files t)
  (after! python
    (add-hook! 'window-state-change-functions 'lubricy/project-set-venv)))

(dolist (flag (doom-module-context-get :flags))
  (load! (symbol-name flag)))
