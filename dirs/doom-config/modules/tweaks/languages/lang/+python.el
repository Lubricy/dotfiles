(after! python
  (poetry-tracking-mode -1)
  (defun lubricy/project-set-venv (&optional window)
    "Set python venv to `.venv'."
    (let ((venv-path
           (file-name-as-directory (concat
                                    (projectile-locate-dominating-file default-directory ".venv")
                                    ".venv"))))
      (when (f-exists? venv-path)
        (setq-local pyvenv-activate venv-path))))
  (add-hook! (org-mode prog-mode) #'lubricy/project-set-venv)
  (after! pyvenv
    (defun lubricy/pyvenv-track-virtualenv ()
      "Set a virtualenv as specified for the current buffer.

If either `pyvenv-activate' or `pyvenv-workon' are specified, and
they specify a virtualenv different from the current one, switch
to that virtualenv."
      (cond
       (pyvenv-activate
        (when (and (not (equal (file-name-as-directory pyvenv-activate)
                               pyvenv-virtual-env))
                   (or (not pyvenv-tracking-ask-before-change)
                       (y-or-n-p (format "Switch to virtualenv %s (currently %s)"
                                         pyvenv-activate pyvenv-virtual-env))))
          (pyvenv-activate pyvenv-activate)))
       (pyvenv-workon
        (when (and (not (equal pyvenv-workon pyvenv-virtual-env-name))
                   (or (not pyvenv-tracking-ask-before-change)
                       (y-or-n-p (format "Switch to virtualenv %s (currently %s)"
                                         pyvenv-workon pyvenv-virtual-env-name))))
          (pyvenv-workon pyvenv-workon)))
       (t
        (pyvenv-deactivate))))
    (add-hook! post-command #'lubricy/pyvenv-track-virtualenv)))
