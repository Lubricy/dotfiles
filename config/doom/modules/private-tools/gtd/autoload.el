;; ;;;###autoload
(defun lubricy/goto-gtd-inbox-file ()
  "Open your private config.el file."
  (interactive)
  (find-file (org-gtd-inbox-path)))
