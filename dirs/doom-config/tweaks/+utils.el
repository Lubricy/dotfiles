(defun org-asset (filename)
  (let ((filedir (format "%s.assets" (file-relative-name buffer-file-name))))
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


(defun org-roam-create-note-from-headline ()
  "Create an Org-roam note from the current headline and jump to it.

Normally, insert the headline’s title using the ’#title:’ file-level property
and delete the Org-mode headline. However, if the current headline has a
Org-mode properties drawer already, keep the headline and don’t insert
‘#+title:'. Org-roam can extract the title from both kinds of notes, but using
‘#+title:’ is a bit cleaner for a short note, which Org-roam encourages."
  (interactive)
  (let ((title (nth 4 (org-heading-components)))
        (has-properties (org-get-property-block)))
    (org-cut-subtree)
    (org-roam-node-find t title)
    (org-paste-subtree)
    (unless has-properties
      (kill-line)
      (while (outline-next-heading)
        (org-promote)))
    (goto-char (point-min))
    (when has-properties
      (kill-line)
      (kill-line))
    (org-capture-finalize)))

(defun workspace-root (&optional index)
  (let* ((index (or index 0))
         (projects (treemacs-workspace->projects (treemacs-current-workspace))))
    (treemacs-project->path (nth index projects))))

(defun workspace-path (relpath &optional index)
  (concat (file-name-as-directory (workspace-root index)) relpath))
