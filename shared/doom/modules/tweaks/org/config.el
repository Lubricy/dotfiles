(after! org
  (add-to-list 'org-modules 'org-id t)
  (add-to-list 'org-modules 'org-habit t)
  (setq org-agenda-search-view-always-boolean t
        org-attach-store-link-p 'attached
        org-attach-dir-relative t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "|" "DONE(d@/@)")
          (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CNCL(c@/!)" "TRASH(s)"))
        org-todo-keyword-faces
        '(("TODO" . +org-todo-onhold)
          ("NEXT" . org-todo)
          ("PROG" . +org-todo-active)
          ("DONE" . org-done)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("CNCL" . +org-todo-cancel)
          ("TRASH" . org-done)))
  (setq org-use-property-inheritance
        (rx (or "PROJECTILE_PROJECT"
                (: "GPTEL_" (* nonl)))))

  (unless (string-match-p "\\.gpg" org-agenda-file-regexp)
    (setq org-agenda-file-regexp
          (replace-regexp-in-string "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?"
                                    org-agenda-file-regexp)))
  (setq +org-capture-todo-file "inbox.org")
  ;; The following setting creates a unique task ID for the heading in the
  ;; PROPERTY drawer when I use C-c l. This allows me to move the task around
  ;; arbitrarily in my org files and the link to it still works.
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (unless (featurep :system 'windows)
    (add-hook! org-mode
               #'envrc-mode))
  (add-hook! org-mode
             #'+org-pretty-mode)
  (add-hook! org-babel-after-execute
             #'shk-fix-inline-images)
  ;; TODO find a more elegant way to require these
  (require 'org-tempo)
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))


;; (after! ob-async
;;   (pushnew! ob-async-no-async-languages-alist "jupyter"))


(load! "+appt")
(load! "+agenda")
(load! "+clock")
(load! "+pretty")
(load! "+timestamp")
(load! "+latex")
(load! "+jupyter")

(dolist (flag (doom-module :tweaks 'org :flags))
  (load! (symbol-name flag)))

(load! "+protocol")
