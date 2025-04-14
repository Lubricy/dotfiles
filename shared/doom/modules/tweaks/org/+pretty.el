(after! org-modern
  (setq!
   org-modern-star 'replace
   org-modern-progress 'nil
   org-modern-keyword '(("results" . "")
                        ("name" . "❯")
                        ("header" . "»")
                        (t . t))
   org-modern-block-name '(("src"  "»" "«")
                           ("quote" "“" "”")
                           ("example" "" "")
                           (t . t))
   org-modern-tag-faces '((t . (:background "#3f6f94"
                                :foreground "white")))))
