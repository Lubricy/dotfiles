(after! org
  (dolist (item
           '(;;; org-capture-protocol
             ("P" "󰿘 Protocol"
              entry (file "inbox.org")
              (file "templates/protocol.org")
              :empy-lines 1
              :clock-in 't
              :clock-resume 't
              )
             ("L" " auto link"
              entry (file+headline "links.org" "Bookmarks")
              (file "templates/auto-link.org")
              :empy-lines 1
              :immediate-finish t)))
    (add-to-list 'org-capture-templates item :append)))
