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
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (if (equal major-mode 'org-agenda-mode)
          (org-agenda-clock-in '(16))
        (lubricy/clock-in-last-task))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)))
          (org-clock-in '(16))
        (lubricy/clock-in-last-task)))))

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
    (org-clock-in nil)))
