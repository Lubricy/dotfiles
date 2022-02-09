(use-package! org-jira
  :after org
  :init
  (setq-default org-jira-working-dir "~/org/jira")
  (add-to-list 'org-agenda-files org-jira-working-dir))
