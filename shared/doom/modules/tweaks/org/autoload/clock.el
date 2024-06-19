;;;###autoload
(defun lubricy/clock-in-last-task ()
  "Clock in the interrupted task if there is one
   Skip the default task and get the next one. "
  (interactive)
  (require 'org-clock)
  (let ((clock-in-to-task
         (cond
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          ((null org-clock-history) (org-id-find org-gtd-idle-id 'marker))
          (t (car org-clock-history)))))
    (save-restriction
      (org-with-point-at clock-in-to-task
        (org-clock-in nil)))))

;;;###autoload
(defun lubricy/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq lubricy/keep-clock-running t)
  (appt-activate 1)
  (cond
   ((derived-mode-p 'org-agenda-mode)
    (org-agenda-clock-in))
   ((derived-mode-p 'org-mode)
    (save-restriction
      (widen)
      (if (org-before-first-heading-p)
          (org-clock-in)
        (lubricy/clock-in-idle))))
   (t (lubricy/clock-in-idle))))

;;;###autoload
(defun lubricy/punch-out ()
  (interactive)
  (setq lubricy/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (appt-activate 0)
  (org-agenda-remove-restriction-lock))

;;;###autoload
(defun lubricy/clock-in-idle ()
  (org-with-point-at
      (org-id-find org-gtd-idle-id 'marker)
    (org-clock-in '(16))))

;;;###autoload
(defun lubricy/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when lubricy/keep-clock-running
            (lubricy/clock-in-idle)))))))
