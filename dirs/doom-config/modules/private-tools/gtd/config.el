;;; tools/gtd/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(after! org
  (add-to-list 'org-modules 'org-id)
  (unless (string-match-p "\\.gpg" org-agenda-file-regexp)
    (setq org-agenda-file-regexp
          (replace-regexp-in-string "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?"
                                    org-agenda-file-regexp)))
  (setq +org-capture-todo-file "inbox.org")
  ;; The following setting creates a unique task ID for the heading in the
  ;; PROPERTY drawer when I use C-c l. This allows me to move the task around
  ;; arbitrarily in my org files and the link to it still works.
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE(p)" "MEETING(m)")))

  (setq org-capture-templates
          '(("i" " item"
             entry (file +org-capture-todo-file)
             "* TODO %?\n%U\n%a\n"
             :clock-in t
             :clock-resume t)
            ("f" " file"
             entry (file +org-capture-todo-file)
             "* %? :NOTE:\n%U\n\n  %i\n  %a"
             :clock-in t
             :clock-resume t)
            ("l" " link"
             entry (file +org-capture-todo-file)
             "* %? :res:\n%U\n\n  %i\n- [ ] %(org-mac-chrome-get-frontmost-url)"
             :clock-in t
             :clock-resume t)
            ("e" " email"
             entry (file +org-capture-todo-file)
             "* NEXT Respond to :@laptop:\nSCHEDULED: %t\n%U\n%a\n- [] %(org-mac-outlook-message-get-links)"
             :clock-in t
             :clock-resume t)
            ("p" " phone"
             entry (file+headline +org-capture-todo-file "Misc")
             "* PHONE %? :misc:phone:\n%U"
             :clock-in t
             :clock-resume t)
            ("m" " meeting"
             entry (file+headline +org-capture-todo-file "Misc")
             "* MEETING with %? :misc:meet:\n%U"
             :clock-in t
             :clock-resume t))))

(define-button-type 'lubricy/crypt-decrypt-button
  'action `(lambda (x) (save-excursion
                   (org-back-to-heading)
                   (org-decrypt-entry)))
  'mouse-action `(lambda (x) (save-excursion
                   (org-back-to-heading)
                   (org-decrypt-entry)))
  'display "Decrypt"
  'help-echo "Decrypt entry")

;; (defun lubricy/org-make-crypt-buttons ()
;;   (interactive)
;;   (let ((org--matcher-tags-todo-only nil))
;;     (org-scan-tags
;;      'lubricy/org-make-crypt-button
;;      (cdr (org-make-tags-matcher org-crypt-tag-matcher))
;;      org--matcher-tags-todo-only)))


(after! org-crypt
  (org-crypt-use-before-save-magic)
  (setq org-crypt-disable-auto-save 'encrypt)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (defadvice! lubricy/org-make-decrypt-button ()
    :after 'org-encrypt-entry
    (pcase (org-at-encrypted-entry-p)
      (`(,beg . ,end)
       (save-excursion
         (make-button beg end :type 'lubricy/crypt-decrypt-button)
         ))
      (_ (lambda (&rest args) nil)))

    ))
