;;;###autoload
(defun shk-fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

;;;###autoload
(defun lubricy/org-babel-node-setenv (&optional root)
  (interactive "DProject root:")
  (let ((root (or root (projectile-locate-dominating-file default-directory "package.json"))))
    (when root
      (let* ((node-modules (concat (file-name-as-directory root) "node_modules"))
             (node-path (getenv "NODE_PATH"))
             (node-paths (if node-path (split-string node-path ":") '())))
        (setenv "NODE_PATH" (string-join (delete-dups (cons node-modules node-paths)) ":"))))))

;;;###autoload
(defun org-asset (filename)
  (concat (file-name-as-directory (org-attach-dir-get-create)) filename))

;;;###autoload
(defun org-attach-asset ()
  (interactive)
  (unless (org-entry-get nil "DIR")
    (org-entry-put nil "DIR" (format "%s.assets" (file-relative-name buffer-file-name))))
  (org-attach-set-directory))

;;;###autoload
(defun org-session (&optional prefix)
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
         (prefix (or prefix (format "session-%s" filename))))
    (format "%s-%s" prefix (secure-hash 'sha1 (buffer-file-name)))))

;;;###autoload
(defun lubricy/clock-in-last-task ()
  "Clock in the interrupted task if there is one
   Skip the default task and get the next one. "
  (interactive)
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
      (widen)
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
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if tags
            (org-agenda-clock-in '(16))
          (lubricy/clock-in-last-task)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
                                        ; Find the tags on the current task
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
