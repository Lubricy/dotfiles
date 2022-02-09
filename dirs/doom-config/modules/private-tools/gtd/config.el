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
        '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "|" "DONE(d)")
          (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CNCL(x@/!)" "TRASH(s)" "CALL(c)" "MEET(m)" "REST(r)")))



  (defun lubricy/switch-task-on-clock-out (task-state)
    "Change a task to 'PROG' when TASK-STATE is 'TODO'."
    (if (string= task-state "PROG")
        "NEXT"
      task-state))
  (defun lubricy/switch-task-on-clock-in (task-state)
    "Change a task to 'PROG' when TASK-STATE is 'TODO'."
    (if (or (string= task-state "TODO") (string= task-state "NEXT"))
        "PROG"
      task-state))

  (setq org-clock-in-switch-to-state #'lubricy/switch-task-on-clock-in)
  (setq org-clock-out-switch-to-state #'lubricy/switch-task-on-clock-out)

  (defun lubricy/org-capture-maybe-create-id ()
    (when (org-capture-get :create-id)
      (org-id-get-create)))
  (add-hook 'org-capture-mode-hook #'lubricy/org-capture-maybe-create-id)


  (defun transform-square-brackets-to-round-ones(string-to-transform)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat
     (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))
  (setq org-capture-templates
        '(("i" " item"
           entry (file +org-capture-todo-file)
           "* TODO %?\n  %i\n%a"
           :clock-in t
           :clock-resume t)
          ("f" " file"
           entry (file +org-capture-todo-file)
           "* %? :NOTE:\n  %i\n%F\n%a"
           :clock-in t
           :clock-resume t)
          ("l" " link"
           entry (file +org-capture-todo-file)
           "* %? :res:\n\n  %i\n%(org-mac-chrome-get-frontmost-url)\n%a"
           :clock-in t
           :clock-resume t)
          ("e" " email"
           entry (file +org-capture-todo-file)
           "* NEXT Respond to %? :@laptop:\nSCHEDULED: %t\n  %i\n%(org-mac-outlook-message-get-links)\n%a"
           :clock-in t
           :clock-resume t)
          ("p" " phone"
           entry (file+headline +org-capture-notes-file "Misc")
           "* PHONE %? :misc:phone:\n%U"
           :clock-in t
           :clock-resume t)
          ("m" " meeting"
           entry (file+headline +org-capture-notes-file "Misc")
           "* MEETING with %? :misc:meet:\n%U"
           :clock-in t
           :clock-resume t)
;;; org-capture
          ("P" " Protocol"
           entry (file +org-capture-todo-file)
           "* %:description :res:link:\n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%a\n"
           :empy-lines 1
           :immediate-finish t)
          ("L" " auto link"
           entry (file+headline "roam/links.org" "Scratch")
           "* %a\n"
           :empy-lines 1
           :immediate-finish t
           :create-id t))))

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


(use-package! org-gtd
  :after org
  :init
  (setq org-gtd-directory "~/org/")
  (setq org-gtd-archive-location
        (lambda ()
          (concat "archive/"
                  (format-time-string "%Y" (current-time))
                  "/%s"
                  "::datetree/")))
  (setq org-archive-location (funcall org-gtd-archive-location))
  :config
  (map! :map org-gtd-process-map
        :g "C-c C-c"  'org-gtd-choose
        :g "C-c C-k"  'org-gtd--stop-processing
        :n [S-return] 'org-gtd-choose
        :n "m"        'org-gtd-choose
        :n "q"        'org-gtd--stop-processing)
  (defun org-gtd--roam-archive ()
    "Process GTD inbox item as a reference item in roam."
    (interactive)
    (with-org-gtd-context (org-roam-create-note-from-headline))
    (org-gtd-process-inbox))
  (transient-define-prefix org-gtd-choose ()
    "Choose how to categorize the current item.
     Note that this function is intended to be used only during inbox processing.
     Each action continues inbox processing, so you may put your emacs in an
     undefined state."
    ["Actionable"
     [("d" "Already Done" org-gtd--quick-action)
      ("a" "Next Action" org-gtd--single-action)]
     [("e" "Someone Else" org-gtd--delegate)
      ("s" "Scheduled Someday" org-gtd--calendar)]
     [("p" "Project (multi-step)" org-gtd--project)]
     ]
    ["Non-actionable"
     [("i" "Incubate" org-gtd--incubate)
      ("r" "Save in Roam" org-gtd--roam-archive)]
     [("t" "Trash" org-gtd--trash)]]
    ["Org GTD"
     ("q"
      "Exit. Stop processing the inbox for now."
      org-gtd--stop-processing)
     ("x"
      "Exit. Continue processing the inbox."
      transient-quit-one)])
  (setq org-gtd-agenda-custom-commands
    '(("g" "Scheduled today and all NEXT items"
       (
        (agenda "" ((org-agenda-span 1)
                    (org-agenda-start-day nil)))
        (todo '("TODO" "NEXT" "PROG") ((org-agenda-overriding-header "All NEXT items")))
        (todo "WAIT" ((org-agenda-todo-ignore-with-date t)
                      (org-agenda-overriding-header "Blocked items")))))))

  (org-gtd-mode))
