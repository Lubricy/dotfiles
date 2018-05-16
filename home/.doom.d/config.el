;;; private/lubricy/config.el -*- lexical-binding: t; -*-

;; (load! +environments)  ; my environments
(load! +bindings)      ; my key bindings

(def-package! docker-tramp
              :after tramp
              :config
              (require 'docker-tramp-compat))

(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2))

(defun project-set-venv ()
  "Set python venv to .venv."
  (let ((venv-path (concat (projectile-project-root) ".venv")))
       (message "Changing to `%s'" venv-path)
       (if (file-directory-p venv-path)
           (pythonic-activate venv-path))))

(after! projectile
  (add-hook 'projectile-after-switch-project-hook 'project-set-venv))

(provide 'config)
;;; config.el ends here
