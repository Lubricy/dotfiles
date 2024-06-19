(use-package! org-jira
  :when (modulep! +jira)
  :after org
  :init
  (setq-default org-jira-working-dir (concat (file-name-as-directory org-directory) "jira"))
  (add-to-list 'org-agenda-files org-jira-working-dir))

(use-package! excorporate
  :when (modulep! +calendar)
  :commands (excorporate exco-connection-iterate)
  :init
  :config
  (setq excorporate-update-diary 'nil)
  (setq-default +excorporate-org-file-name
                (concat (file-name-as-directory org-directory) "calendar.org")))

(use-package! excorporate-org
  :when (modulep! +calendar)
  :after (excorporate org)
  :config
  ;; HACK
  (defun exco-org-insert-meeting (subject start end location
                                          main-invitees optional-invitees
                                          &optional item-identifier organizer)
    "Insert a scheduled meeting.
     SUBJECT is a string, the subject of the meeting.  START is the
     meeting start time in Emacs internal date time format, and END is
     the end of the meeting in the same format.  LOCATION is a string
     representing the location.  MAIN-INVITEES and OPTIONAL-INVITEES
     are the requested participants.  ITEM-IDENTIFIER is the opaque
     item identifier.  ORGANIZER is a string, the email address of the
     meeting organizer."
    (let* ((now (current-time)))
      (insert (format "** MEET %s\n" subject))
      (org-set-property "Identifier" (format "%S" item-identifier))
      (org-set-property "Duration" (format "%d minutes" (round (/ (float-time (time-subtract end start)) 60.0))))
      (when location
        (org-set-property "Location" location))
      (org-set-property "Organizer" organizer)
      (org-set-property "Retrieved" (format-time-string "[%Y-%m-%d %a %H:%M]" (current-time)))
      (insert (format-time-string "<%Y-%m-%d %a %H:%M>" start) "--"
              (format-time-string "<%Y-%m-%d %a %H:%M>" end) "\n")
      (when main-invitees
        (insert "*** Invitees:\n")
        (exco-org-insert-invitees main-invitees))
      (when optional-invitees
        (insert "*** Optional invitees:\n")
        (exco-org-insert-invitees optional-invitees)))))
(use-package! confluence
  :when (modulep! +confluence)
  :defer t
  :commands (confluence-search confluence-get-page))

(map! (:leader
       (:prefix "d"
        :desc "fetch mail" "m" #'+exco-org-today
        :desc "fetch jira" "j" #'org-jira-get-issues)))
