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

