;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(load! "+bindings")
(load! "+evil-commands")

;; I've swapped these keys on my keyboard
(setq x-meta-keysym         'alt
      x-alt-keysym          'meta
      mac-option-modifier   'alt
      mac-command-modifier  'meta
      mac-function-modifier 'control

      user-mail-address "lubricy@gmail.com"
      user-full-name    "Lubricy Fibber")

;; company auto-complete
(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2))

(after! yasnippet
  (push "~/.doom.d/snippets" yas-snippet-dirs))

(defun project-set-venv ()
  "Set python venv to `.venv'."
  (when (projectile-project-p)
    (let ((venv-path (file-name-as-directory (concat (projectile-project-root) ".venv"))))
      (when (and (not (equal python-shell-virtualenv-path venv-path))
                 (file-directory-p venv-path))
        (message "Changing to `%s'" venv-path)
        (pyvenv-activate venv-path)))))

(after! projectile
  (add-hook 'python-mode-hook 'project-set-venv)
  ;; (add-hook 'hack-local-variables-hook 'project-set-venv)
  (setq projectile-project-search-path '("~/Projects/")))

(after! treemacs
  (setq treemacs-collapse-dirs 20))

(defun shk-fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(after! ob-async
  (pushnew! ob-async-no-async-languages-alist "jupyter"))

(after! org
  ;; Split up the search string on whitespace
  ;; (after!
  ;;   (dolist (lang '(python julia R))
  ;;     (org-babel-jupyter-make-language-alias nil lang)))
  (setq org-agenda-search-view-always-boolean t)
  (add-hook 'org-babel-after-execute-hook 'shk-fix-inline-images)
  (add-hook 'ob-async-pre-execute-src-block-hook
          '(lambda ()
            (require 'docker-tramp))))


(provide 'config)
;;; config.el ends here
