;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(load! "+bindings")
(load! "+evil-commands")
(load! "+local-config")
(load! "+org")

;; I've swapped these keys on my keyboard
(setq x-meta-keysym         'alt
      x-alt-keysym          'meta
      mac-option-modifier   'alt
      mac-command-modifier  'meta
      mac-function-modifier 'control

      user-mail-address "lubricy@gmail.com"
      user-full-name    "Lubricy Fibber")

;; treemacs to show git directory highlights
(setq +treemacs-git-mode 'deferred)

;; company auto-complete
(after! company
  (add-to-list 'company-backends 'company-files)

  (setq company-idle-delay 0.01
        company-minimum-prefix-length 1))

(after! anaconda-mode
  (set-company-backend! 'anaconda-mode '(company-anaconda :with company-tabnine)))

(after! tide
  (set-company-backend! 'tide-mode '(company-tabnine :with company-tide)))

(after! yasnippet
  (push "~/.doom.d/snippets" yas-snippet-dirs))

(defun lubricy/babel-ansi ()
  (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (ansi-color-context-region nil))
          (ansi-color-apply-on-region beg end))))))

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

(defun lubricy/org-babel-node-setenv ()
  (let ((root (projectile-locate-dominating-file default-directory "package.json")))
    (when root
      (let* ((node-modules (concat (file-name-as-directory root) "node_modules"))
             (node-path (getenv "NODE_PATH"))
             (node-paths (if node-path (split-string node-path ":") '())))
          (setenv "NODE_PATH" (string-join (delete-dups (cons node-modules node-paths)) ":"))))))

(after! projectile
  (setq projectile-project-search-path '("~/Projects/"))
  (add-hook! 'window-state-change-functions 'lubricy/project-set-venv))

(after! treemacs
  (setq treemacs-collapse-dirs 20))

(defun shk-fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(after! ob-async
  (pushnew! ob-async-no-async-languages-alist "jupyter"))

(after! org
  (setq org-agenda-search-view-always-boolean t)
  (add-hook 'org-mode-hook 'lubricy/org-babel-node-setenv)
  (add-hook 'org-babel-after-execute-hook 'lubricy/babel-ansi)
  (add-hook 'org-babel-after-execute-hook 'shk-fix-inline-images)
  (add-hook 'ob-async-pre-execute-src-block-hook
          '(lambda ()
            (require 'docker-tramp))))


(provide 'config)
;;; config.el ends here
