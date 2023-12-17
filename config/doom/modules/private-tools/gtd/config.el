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
      (insert "* " headline   "\n"
              ":PROPERTIES:" "\n"
              ":TRIGGER: relatives(forward-no-wrap todo-only 1 no-sort) todo!(NEXT)" "\n"
              ":CATEGORY: " (projectile-project-name project) "\n"
              ":ORG_GTD: Projects" "\n"
              ":END:" "\n\n"
              "[[elisp:(projectile-switch-project-by-name \"" (abbreviate-file-name project) "\")][Open Project]]" "\n\n")
      (search-backward headline)
      (move-beginning-of-line 'nil))))

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
  (setq org-archive-location
        (concat "archive/"
                (format-time-string "%Y" (current-time))
                "/%s"
                "::datetree/"))

  (setq org-gtd-update-ack "3.0.0")
  :config
  (setq org-gtd-archive-file-format "archive/%s/gtd_archive.org")
  (setq org-edna-use-inheritance 1)
  (org-edna-mode 1)
  (setq org-gtd-engage-prefix-width 20)
  (defun +org-set-roam-tags-command ()
    (unless (org-gtd-organize-type-member-p
             '(quick-action
               project-task))
      (org-back-to-heading)
      (let* ((input (org-make-tag-string (org-get-tags)))
             (crm-separator "[ \t]*:[ \t]*"))
        (org-set-tags
         (completing-read-multiple
          "Tags: "
          (org-roam-tag-completions)
          'nil 'nil input)))))
  (setq org-gtd-organize-hooks '(+org-set-roam-tags-command))
  (map! :map org-gtd-clarify-map
        :g "C-c C-c"  'org-gtd-organize
        :g "C-c C-k"  'kill-current-buffer
        :n "Z Z"  'org-gtd-organize
        :n "Z Q"  'kill-current-buffer
        :n [S-return] 'org-gtd-organize)

  (defun lubricy/org-gtd-knowledge--apply ()
    "Once the user has filed this knowledge, we can execute this logic."
    (org-todo 'none)
    (setq-local org-gtd--organize-type 'knowledge)
    (org-gtd-organize-apply-hooks)
    (+org-roam-refile-or-create))

  (setq org-gtd-knowledge-func #'lubricy/org-gtd-knowledge--apply)
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
     [("t" "Trash" org-gtd-trash)]]))


;; (setq org-capture-templates (butlast org-capture-templates 2))
