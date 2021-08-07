(defun lubricy/project-set-venv (&optional window)
  "Set python venv to `.venv'."
  (when (projectile-project-p)
      (if (equal major-mode 'python-mode)
          (let ((venv-path
                 (file-name-as-directory (concat
                                          (projectile-locate-dominating-file default-directory ".venv")
                                          ".venv"))))
            (if (file-directory-p venv-path)
                (when (not (equal python-shell-virtualenv-root venv-path))
                  (pyvenv-activate venv-path))
                (pyvenv-deactivate)))
          (pyvenv-deactivate))))


(after! projectile
  (setq projectile-project-search-path '("~/Projects/"))
  (add-hook! 'window-state-change-functions 'lubricy/project-set-venv))
