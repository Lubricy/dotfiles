;;; private/lubricy/config.el -*- lexical-binding: t; -*-

(load! "+bindings")
(when (featurep 'evil)
  (load! "+evil-commands"))

;; --- <ex-commands> ------------------------------

;; --- packages -----------------------------------

(def-package! docker-tramp
              :after tramp)
(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2))

;(def-package-hook! python
;  :post-config
;  (set-company-backend 'python-mode '(company-anaconda company-dabbrev-code company-yasnippet))
;  t)

(defun project-set-venv ()
  "Set python venv to `.venv'."
  (when (projectile-project-p)
    (let ((venv-path (file-name-as-directory (concat (projectile-project-root) ".venv"))))
      (when (and (not (equal python-shell-virtualenv-path venv-path))
                 (file-directory-p venv-path))
        (message "Changing to `%s'" venv-path)
        (setq-local python-shell-virtualenv-path venv-path)
        (setq-local flycheck-python-pycompile-executable (concat venv-path "bin/python"))
        (setenv "PYTHONPATH" (projectile-project-root))))))

(after! projectile
  (add-hook 'python-mode-hook 'project-set-venv))

(after! python
  (add-hook 'python-mode-hook (λ! (setq indent-tabs-mode nil)
                                  (setq tab-width 4))))

(provide 'config)
;;; config.el ends here
