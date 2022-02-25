;;;###autoload
(defun org-asset (filename)
  (let ((filedir (format "%s.assets" (file-relative-name buffer-file-name))))
    (make-directory filedir 't)
    (format "%s/%s" filedir filename)))

(defun org-session (&optional prefix)
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
         (prefix (or prefix (format "session-%s" filename))))
    (format "%s-%s" prefix (secure-hash 'sha1 (buffer-file-name)))))
;;;###autoload
(defun workspace-root (&optional index)
  (let* ((index (or index 0))
         (projects (treemacs-workspace->projects (treemacs-current-workspace))))
    (treemacs-project->path (nth index projects))))
;;;###autoload
(defun workspace-path (relpath &optional index)
  (concat (file-name-as-directory (workspace-root index)) relpath))
