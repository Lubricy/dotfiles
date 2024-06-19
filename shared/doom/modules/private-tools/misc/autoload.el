;;;###autoload
(defun workspace-root (&optional index)
  (let* ((index (or index 0))
         (projects (treemacs-workspace->projects (treemacs-current-workspace))))
    (treemacs-project->path (nth index projects))))
;;;###autoload
(defun workspace-path (relpath &optional index)
  (concat (file-name-as-directory (workspace-root index)) relpath))
