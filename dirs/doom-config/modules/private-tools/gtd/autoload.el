;;;###autoload
(defun lubricy/punch-in ()
  (interactive)
  (when (featurep! :private-tools corporate)
    (+exco-org-today)
    (appt-activate 1))
  (when (featurep! :private-tools corporate +jira)
    (with-temp-buffer
      (call-interactively #'org-jira-get-issues)))
  (lubricy/clock-in-default))


;;;###autoload
(defun lubricy/punch-out ()
  (interactive)
  (org-clock-out)
  (appt-activate 0))
;;;###autoload
(defun lubricy/clock-in-default ()
  (interactive)
  (when (bound-and-true-p org-gtd-idle-id)
    (org-with-point-at (org-id-find org-gtd-idle-id 'marker)
      (org-clock-in '(16)))))
;;;###autoload
(defun +org-roam-refile-or-create ()
  "Create an Org-roam note from the current headline and jump to it.

Normally, insert the headline’s title using the ’#title:’ file-level property
and delete the Org-mode headline. However, if the current headline has a
Org-mode properties drawer already, keep the headline and don’t insert
‘#+title:'. Org-roam can extract the title from both kinds of notes, but using
‘#+title:’ is a bit cleaner for a short note, which Org-roam encourages."
  (interactive)
  (let* ((regionp (org-region-active-p))
         (region-start (and regionp (region-beginning)))
         (region-end (and regionp (region-end)))
         (_title (nth 4 (org-heading-components)))
         (title (or (with-temp-buffer
                      (insert _title)
                      (beginning-of-buffer)
                      (let* ((link (org-element-link-parser))
                             (beg (org-element-property :contents-begin link))
                             (end (org-element-property :contents-end link)))
                        (buffer-substring beg end)))
                    _title))
         (node (org-roam-node-read title))
         (file (org-roam-node-file node))
         nbuf level reversed)
    (if regionp
        (progn
          (org-kill-new (buffer-substring region-start region-end))
          (org-save-markers-in-region region-start region-end))
      (progn
        (if (org-before-first-heading-p)
            (org-roam-demote-entire-buffer))
        (org-copy-subtree 1 nil t)))
    (if file
        (setq nbuf (or (find-buffer-visiting file)
                       (find-file-noselect file)))
      (org-roam-capture-
       :node node
       :info )
      (setq nbuf (current-buffer)))
    (with-current-buffer nbuf
      (org-with-wide-buffer
       (goto-char (or (org-roam-node-point node) (point-max)))
       (setq level (org-get-valid-level (funcall outline-level) 1)
             reversed (org-notes-order-reversed-p))
       (goto-char
        (if reversed
            (or (outline-next-heading) (point-max))
          (or (save-excursion (org-get-next-sibling))
              (org-end-of-subtree t t)
              (point-max))))
       (unless (bolp) (newline))
       (org-paste-subtree level nil nil t)
       (when file
         (org-id-get-create))
       (and org-auto-align-tags
            (let ((org-loop-over-headlines-in-active-region nil))
              (org-align-tags)))
       (when (fboundp 'deactivate-mark) (deactivate-mark)))
      (unless file
        (org-capture-finalize)))
    (if regionp
        (delete-region (point) (+ (point) (- region-end region-start)))
      (org-preserve-local-variables
       (delete-region
        (and (org-back-to-heading t) (point))
        (min (1+ (buffer-size)) (org-end-of-subtree t t) (point)))))))
