(after! org
  (setq
   org-capture-templates
   '(("i" " item" entry
      (file "inbox.org")
      (file "templates/item.org")
      :empy-lines 1
      :clock-in 't
      :clock-resume 't)
     ("E" " email"
      (file "inbox.org")
      (file "templates/email.org")
      :empy-lines 1))
   ))
