(when IS-MAC
  (after! epa
    ;; uses pinentry-osx
    (setq epg-pinentry-mode 'ask))

  (after! unicode-fonts
    (dolist (unicode-block '("Mathematical Alphanumeric Symbols"))
      (pushnew "STIXGeneral" (cadr (assoc unicode-block unicode-fonts-block-font-mapping)))))

  (use-package! org-mac-link
    :after org
    :config
    (setq org-mac-grab-Acrobat-app-p nil) ; Disable grabbing from Adobe Acrobat
    (setq org-mac-grab-devonthink-app-p nil) ; Disable grabbinb from DevonThink
    ))
