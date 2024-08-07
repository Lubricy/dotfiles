(after! org
  (dolist (item
           '(;;; org-capture-protocol
             ("P" "󰿘 Protocol"
              entry (file "inbox.org")
              (file "templates/protocol.org")
              :empty-lines 1
              :clock-in 't
              :clock-resume 't
              )
             ("L" " auto link"
              entry (file+headline "roam/links.org" "Scratch")
              (file "templates/auto-link.org")
              :empty-lines 1
              :immediate-finish t)))
    (add-to-list 'org-capture-templates item :append)))
