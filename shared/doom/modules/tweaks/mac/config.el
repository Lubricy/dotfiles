(when IS-MAC
  (after! epa
    ;; uses pinentry-osx
    (setq epg-pinentry-mode 'ask))

  ;; (when (fboundp 'set-fontset-font)
  ;;   (set-fontset-font t 'mathematical "STIX Two Math"))
  (setq use-default-font-for-symbols nil)
  (setq doom-font (font-spec :family "AnonymicePro Nerd Font" :size 14))
  ;; (cl-pushnew "STIX Two Math" doom-emoji-fallback-font-families :test #'string=)
  (after! unicode-fonts
    (dolist (unicode-block '("Letterlike Symbols"
                             "Mathematical Alphanumeric Symbols"
                             "Mathematical Operators"
                             "Miscellaneous Mathematical Symbols-A"
                             "Miscellaneous Mathematical Symbols-B"
                             "Miscellaneous Symbols"
                             "Miscellaneous Symbols and Arrows"
                             "Miscellaneous Symbols and Pictographs"))
      (cl-pushnew "STIX Two Math" (cadr (assoc unicode-block unicode-fonts-block-font-mapping)) :test #'string=)))

  (use-package! org-mac-link
    :after org
    :config
    (setq org-mac-grab-Acrobat-app-p nil) ; Disable grabbing from Adobe Acrobat
    (setq org-mac-grab-devonthink-app-p nil) ; Disable grabbinb from DevonThink
    ))
