(defun org-asset (filename)
  (let ((filedir (format "%s.assets" (buffer-file-name))))
    (make-directory filedir 't)
    (format "%s/%s" filedir filename)))

(defun org-session (&optional prefix)
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
         (prefix (or prefix (format "session-%s" filename))))
    (format "%s-%s" prefix (secure-hash 'sha1 (buffer-file-name)))))

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'tree))
