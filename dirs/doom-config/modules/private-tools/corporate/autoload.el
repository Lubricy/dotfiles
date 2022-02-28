;;;###autoload
(defun +exco-org-today ()
  "Show meetings for the date today."
  (interactive)
  (destructuring-bind (_ _ _ day month year &rest) (decode-time)
    (exco-connection-iterate #'exco-org-initialize-buffer
                             (lambda (identifier callback)
                               (exco-org-insert-headline identifier
                                                         month day year)
                               (exco-get-meetings-for-day identifier
                                                          month day year
                                                          callback))
                             #'exco-org-insert-meetings
                             (lambda ()
                               (exco-org-finalize-buffer)
                               (with-current-buffer
                                   (get-buffer-create excorporate-org-buffer-name)
                                 (write-region
                                  (point-min) (point-max)
                                  +excorporate-org-file-name))))))
