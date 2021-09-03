;;; tools/jira/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(use-package! ejira
  :after org
  :config
  ;; Tries to auto-set custom fields by looking into /editmeta
  ;; of an issue and an epic.
  (add-hook 'jiralib2-post-login-hook #'ejira-guess-epic-sprint-fields)

  ;; They can also be set manually if autoconfigure is not used.
  ;; (setq ejira-sprint-field       'customfield_10001
  ;;       ejira-epic-field         'customfield_10002
  ;;       ejira-epic-summary-field 'customfield_10004)

  (require 'ejira-agenda)
  (add-to-list
   'org-link-abbrev-alist
   (cons "jira" (s-concat jiralib2-url "/browse/%s")))

  (defun +ejira-insert-link (id)
    (interactive (list (read-string "Jira Story: " (s-concat ejira-scrum-project "-"))))
    (org-insert-link nil (format "jira:%s" id) "@")
    (org-insert-link nil (format "id:%s" id) id))

    ;; Make the issues visisble in your agenda by adding `ejira-org-directory'
    ;; into your `org-agenda-files'.
  ;;(add-to-list 'org-agenda-files ejira-org-directory)

          ;; Add an agenda view to browse the issues that
  (map! :map (ejira-mode-map)
        "C-c C-t" #'ejira-progress-issue)
  (org-add-agenda-custom-command
     '("j" "My JIRA issues"
       ((ejira-jql "resolution = unresolved and assignee = currentUser()"
                       ((org-agenda-overriding-header "Assigned to me")))))))
