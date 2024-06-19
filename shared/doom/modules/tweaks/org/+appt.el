(after! appt
  (defun lubricy/appt-display-interval-fn (time-remain)
    (message (pp time-remain))
    (= (mod time-remain 5) 1))
  (defun lubricy/send-notification (title msg)
    (let ((notifier-path (executable-find "alerter")))
      (start-process
       "Appointment Alert"
       "*Appointment Alert*"
       notifier-path
       "-message" msg
       "-title" title
       "-group" "Emacs Appointments")))
  (defun lubricy/appt-display-native (min-to-app new-time msg)
    ;; min-to-app is a string
    (let ((remain (string-to-number min-to-app)))
      (when (lubricy/appt-display-interval-fn remain)
        (lubricy/send-notification
         (format "%s" msg)
         (format "In %s minutes" min-to-app) ; Title
         ))
      (when (= remain 0)
        (lubricy/send-notification
         (format "%s" msg)
         "Now"              ; Title
         ))))
  
  (setq appt-display-interval '1
        appt-message-warning-time '6 ;; send first warning 6 minutes before appointment
        appt-display-mode-line 'nil   ;; don't show in the modeline
        appt-display-format 'window) ;; pass warnings to the designated window function

  (setq appt-disp-window-function #'lubricy/appt-display-native))
