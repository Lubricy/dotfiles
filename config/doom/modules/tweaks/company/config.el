(after! company
  (cl-pushnew 'company-files (default-value 'company-backends))

  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1))

(when (modulep! +tabnine)
  (use-package! company-tabnine
    :after company
    :config
    (setq +lsp-company-backends '(company-tabnine company-capf))
    (cl-pushnew 'company-tabnine (default-value 'company-backends))
    )


  (after! anaconda-mode
    (set-company-backend! 'anaconda-mode '(:separate company-anaconda :with company-tabnine)))

  (after! tide
    (set-company-backend! 'tide-mode '(:separate company-tide :with company-tabnine))))
