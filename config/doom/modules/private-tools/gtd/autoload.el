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
                        (and beg end (buffer-substring beg end))))
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
      (if file
          (save-buffer)
        (org-capture-finalize)))
    (if regionp
        (delete-region (point) (+ (point) (- region-end region-start)))
      (org-preserve-local-variables
       (delete-region
        (and (org-back-to-heading t) (point))
        (min (1+ (buffer-size)) (org-end-of-subtree t t) (point)))))))
