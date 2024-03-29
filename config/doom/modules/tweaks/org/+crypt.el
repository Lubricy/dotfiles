;;;###if (modulep! +crypt)

(after! org-crypt
  ;; HACK https://lists.endsoftwarepatents.org/archive/html/bug-gnu-emacs/2014-05/msg00597.html
  (fset 'epg-wait-for-status 'ignore)
  (org-crypt-use-before-save-magic)
  (setq org-crypt-disable-auto-save 'encrypt)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (defadvice! lubricy/org-make-decrypt-button ()
    :after 'org-encrypt-entry
    (pcase (org-at-encrypted-entry-p)
      (`(,beg . ,end)
       (save-excursion
         (let ((+beg (- (save-excursion (goto-char beg) (forward-line  1) (beginning-of-line) (point)) 6)))
           (make-button
            beg +beg
            'face 'org-drawer
            'action (cmd!
                     ;; (fset 'epg-wait-for-status-backup 'epg-wait-for-status)
                     (org-decrypt-entry)
                     ;; (fset 'epg-wait-for-status 'epg-wait-for-status-backup)
                     )
            'mouse-action (cmd!
                           ;; (fset 'epg-wait-for-status-backup 'epg-wait-for-status)
                           (org-decrypt-entry)
                           ;; (fset 'epg-wait-for-status 'epg-wait-for-status-backup)
                           ))
           (org-flag-region +beg end t 'outline))
         ))
      (_ nil)))
  (add-hook! org-mode
    (when (and (buffer-file-name (current-buffer))
               (file-exists-p (buffer-file-name (current-buffer))))
      (org-map-entries #'lubricy/org-make-decrypt-button))))
