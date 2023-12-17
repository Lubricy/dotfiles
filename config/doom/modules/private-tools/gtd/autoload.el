;;;###autoload
(defun lubricy/goto-gtd-inbox-file ()
  "Open your private config.el file."
  (interactive)
  (find-file (org-gtd-inbox-path)))

;;;###autoload
(defun +org-gtd-dwim (arg)
  (interactive "P")
  (cond
   (arg
    (org-capture))
   ((derived-mode-p 'org-agenda-mode)
    (org-gtd-clarify-agenda-item))
   ((derived-mode-p 'org-mode)
    (org-gtd-clarify-item))
   ('t (org-capture))))

;;;###autoload
(defun +org-set-cleanup-inbox-todo (&optional state id)
  (let* ((state (or state "TODO"))
         (id (or id "cleanup-inbox"))
         (marker (org-id-find id 'marker))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    (with-current-buffer buffer
      (goto-char pos)
      (org-todo state))))

;;;###autoload
(defun +org-cleanup-inbox-todo ()
  (if (= 0 (length (org-map-entries nil nil 'file)))
      (+org-set-cleanup-inbox-todo "DONE")
    (+org-set-cleanup-inbox-todo "NEXT")))

;;;###autoload
(defun org-gtd-project-engage ()
  "Agenda view with all invalid Calendar actions."
  (interactive)
  (with-org-gtd-context
      (let ((org-agenda-custom-commands
             '(("g" "foobar"
                ((tags "ORG_GTD=\"Habits\""
                       ((org-agenda-skip-function
                         'org-gtd-skip-unless-timestamp-empty-or-invalid)
                        (org-agenda-skip-additional-timestamps-same-entry t))))))))
        (org-agenda nil "g"))))
