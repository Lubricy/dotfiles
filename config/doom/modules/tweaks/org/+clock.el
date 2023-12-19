(after! org
  (setq org-clock-persist t)
  (setq lubricy/keep-clock-running nil)
  (setq  lubricy/task-history nil)
  (add-hook! org-clock-in
    (when (bound-and-true-p lubricy/keep-clock-running)
      (let ((task (copy-marker (point))))
        (setq lubricy/task-history
              (cons task (remove task lubricy/task-history))))))
  (add-hook! org-clock-out
    (when (and (bound-and-true-p lubricy/keep-clock-running)
               (not org-clock-clocking-in)
               (marker-buffer org-clock-default-task)
               (not org-clock-resolving-clocks-due-to-idleness))
      (pop lubricy/task-history)
      (if-let ((task (car lubricy/task-history)))
          (org-with-point-at task
            (org-clock-in))
        (lubricy/clock-in-idle))))
  (unless (modulep! :private-tools gtd)
    (defun lubricy/switch-task-on-clock-out (task-state)
      "Change a task to 'NEXT' when TASK-STATE is 'NEXT'."
      (if (string= task-state "PROG")
          "NEXT"
        task-state))
    (defun lubricy/switch-task-on-clock-in (task-state)
      "Change a task to 'PROG' when TASK-STATE is 'TODO'."
      (if (and
           (buffer-file-name)
           (not (s-contains? "inbox" (buffer-file-name)))
           (or (string= task-state "TODO") (string= task-state "NEXT")))
          "PROG"
        task-state))

    (setq org-clock-in-switch-to-state #'lubricy/switch-task-on-clock-in)
    (setq org-clock-out-switch-to-state #'lubricy/switch-task-on-clock-out)))
