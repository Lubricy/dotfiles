(after! org
  (setq
   org-capture-templates
   '(("i" " Item" entry
      (file "inbox.org")
      (file "templates/item.org")
      :empy-lines 1
      :clock-in 't
      :clock-resume 't)
     ("C" "󰅌 Clipboard" entry
      (file "inbox.org")
      (file "templates/clipboard.org")
      :empy-lines 1)
     ("E" " Email" entry
      (file "inbox.org")
      (file "templates/email.org")
      :empy-lines 1)))

  (dolist (item
           `(;;; org-capture-protocol
             ("p" " Project"
              entry (function ,(lambda () (+org-capture-per-project "notes.org" 'nil)))
              (file "templates/item.org")
              :empy-lines 1
              :clock-in 't
              :clock-resume 't
              :before-finalize (+org-capture-per-project-fix-todo)
              )
             ("o" " Other Project..."
              entry (function ,(lambda () (+org-capture-per-project "notes.org" 'nil 't)))
              (file "templates/item.org")
              :empy-lines 1
              :clock-in 't
              :clock-resume 't
              :before-finalize (+org-capture-per-project-fix-todo)
              )))
    (add-to-list 'org-capture-templates item :append)))
