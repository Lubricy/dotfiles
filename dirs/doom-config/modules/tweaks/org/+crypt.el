;;;###if (featurep! +crypt)

(after! org-crypt
  (org-crypt-use-before-save-magic)
  (setq org-crypt-disable-auto-save 'encrypt)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (defadvice! lubricy/org-make-decrypt-button ()
    :after 'org-encrypt-entry
    (pcase (org-at-encrypted-entry-p)
      (`(,beg . ,end)
       (save-excursion
         (let ((+beg (save-excursion (goto-char beg) (forward-line  1) (beginning-of-line) (point)))
               (+end (save-excursion (goto-char end) (forward-line -2) (end-of-line) (point))))
           (make-button beg (1- +beg) 'action (cmd! (org-decrypt-entry)))
           (make-button +beg +end 'display "-----ENCRYPTED BODY-----" 'action (cmd! (org-decrypt-entry)))
           (make-button (1+ +end) end 'action (cmd! (org-decrypt-entry))))
         ))
      (_ nil)))
  (add-hook! org-mode
             (org-map-entries #'lubricy/org-make-decrypt-button)))
