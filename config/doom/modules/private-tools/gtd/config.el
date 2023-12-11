;;; tools/gtd/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(defvar org-capture-per-project-headline "Tasks")

(defun +org-capture-per-project (path &optional headline other)
  (let ((headline (or headline org-capture-per-project-headline))
        (project (if other
                     (let ((projects (projectile-relevant-known-projects)))
                       (if projects
                           (projectile-completing-read
                            "Switch to project: " projects)
                         (user-error "There are no known projects")))
                   (or (projectile-project-p) (user-error "There are no known projects")))))
    (set-buffer (org-capture-target-buffer (expand-file-name path project)))
    ;; Org expects the target file to be in Org mode, otherwise
    ;; it throws an error.  However, the default notes files
    ;; should work out of the box.  In this case, we switch it to
    ;; Org mode.
    (unless (derived-mode-p 'org-mode)
      (org-display-warning
       (format "Capture requirement: switching buffer %S to Org mode"
               (current-buffer)))
      (org-mode))
    (org-capture-put-target-region-and-position)
    (widen)
    (goto-char (point-min))
    (if (re-search-forward (format org-complex-heading-regexp-format
                                   (regexp-quote headline))
                           nil t)
        (forward-line 0)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "* " headline "\n"
              ":PROPERTIES:" "\n"
              ":TRIGGER: relatives(forward-no-wrap todo-only 1 no-sort) todo!(NEXT)" "\n"
              ":CATEGORY: " (projectile-project-name project) "\n"
              ":ORG_GTD: Projects" "\n"
              ":END:" "\n")
      (forward-line -6))))

(defun +org-capture-per-project-fix-todo ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (format org-complex-heading-regexp-format
                   (regexp-quote org-capture-per-project-headline))
           nil t)
      (forward-line 0)
      (org-gtd-projects-fix-todo-keywords (point-marker)))))
(use-package! org-gtd
  :after org
  :init
  (setq org-gtd-directory "~/org/")
  (setq org-gtd-refile-to-any-target 'nil)
  (setq org-gtd-archive-location
        (lambda ()
          (concat "archive/"
                  (format-time-string "%Y" (current-time))
                  "/%s"
                  "::datetree/")))
  (setq org-archive-location (funcall org-gtd-archive-location))

  (setq org-gtd-update-ack "3.0.0")
  :config
  (setq org-edna-use-inheritance 1)
  (org-edna-mode 1)
  (setq org-gtd-engage-prefix-width 20)
  (map! :map org-gtd-clarify-map
        :g "C-c C-c"  'org-gtd-organize
        :g "C-c C-k"  'kill-current-buffer
        :n "Z Z"  'org-gtd-organize
        :n "Z Q"  'kill-current-buffer
        :n [S-return] 'org-gtd-organize)


  (transient-define-prefix org-gtd-organize ()
    "Choose how to categorize the current item."
    ["Actionable"
     [("a" "Quick action" org-gtd-quick-action)
      ("s" "Single action" org-gtd-single-action)]
     [("d" "Delegate" org-gtd-delegate)
      ("c" "Calendar" org-gtd-calendar)
      ("h" "Habit" org-gtd-habit)]]
    [("n" "New project (multi-step)" org-gtd-project-new)
     ("p" "Add this task to an existing project" org-gtd-project-extend)]
    ["Non-actionable"
     [("i" "Incubate" org-gtd-incubate)
      ("k" "Knowledge to be stored" org-gtd-knowledge)]
     [("t" "Trash" org-gtd-trash)]])


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

;; (setq org-capture-templates (butlast org-capture-templates 2))
