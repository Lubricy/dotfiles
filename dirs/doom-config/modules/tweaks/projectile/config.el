(after! projectile
  (setq projectile-project-search-path '("~/Projects/"))
  (add-hook! 'window-state-change-functions 'lubricy/project-set-venv))
