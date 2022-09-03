(after! org
  (defvar org-babel-eval-verbose t
    "A non-nil value makes `org-babel-eval' display")
  (defun org-babel-eval (command query)
    "Run COMMAND on QUERY.
     Writes QUERY into a temp-buffer that is processed with
     `org-babel--shell-command-on-region'.  If COMMAND succeeds then return
     its results, otherwise display STDERR with
     `org-babel-eval-error-notify'."
    (let ((error-buffer (get-buffer-create " *Org-Babel Error*")) exit-code)
      (with-current-buffer error-buffer (erase-buffer))
      (with-temp-buffer
        (insert query)
        (setq exit-code
              (org-babel--shell-command-on-region
               command error-buffer))
        (if (or (not (numberp exit-code)) (> exit-code 0)
                (and org-babel-eval-verbose (> (buffer-size error-buffer) 0))) ; new condition
            (progn
              (with-current-buffer error-buffer
                (org-babel-eval-error-notify exit-code (buffer-string)))
              (save-excursion
                (when (get-buffer org-babel-error-buffer-name)
                  (with-current-buffer org-babel-error-buffer-name
                    (unless (derived-mode-p 'compilation-mode)
                      (compilation-mode))
                    ;; Compilation-mode enforces read-only, but Babel expects the buffer modifiable.
                    (setq buffer-read-only nil))))
              nil))
        (buffer-string)))))
