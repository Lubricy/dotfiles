;; company auto-complete

(after! company
  (add-to-list 'company-backends 'company-files)

  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1))

(after! anaconda-mode
  (set-company-backend! 'anaconda-mode '(company-anaconda :with company-tabnine)))

(after! tide
  (set-company-backend! 'tide-mode '(company-tabnine)))

(after! yasnippet
  (push "~/.doom.d/snippets" yas-snippet-dirs))
