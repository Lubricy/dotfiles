(after! org
  (setq org-agenda-search-view-always-boolean t
        org-attach-store-link-p 'attached
        org-attach-dir-relative t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "|" "DONE(d)")
          (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CNCL(x@/!)" "TRASH(s)")
          (sequence "CALL(c)" "MEET(m)" "|" "REST(r)"))
        org-todo-keyword-faces
        '(("TODO" . +org-todo-onhold)
          ("NEXT" . org-todo)
          ("PROG" . +org-todo-active)
          ("DONE" . org-done)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("CNCL" . +org-todo-cancel)
          ("TRASH" . org-done)
          ("MEET" . +org-todo-active)
          ("CALL" . +org-todo-active)
          ("REST" . +org-todo-active)))
  (add-hook! org-mode
             #'lubricy/org-babel-node-setenv
             #'+org-pretty-mode)
  (add-hook! org-babel-after-execute
             #'lubricy/babel-ansi
             #'shk-fix-inline-images)
  ;; TODO find a more elegant way to require these
  (require 'org-tempo)
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))


(after! ob-async
  (pushnew! ob-async-no-async-languages-alist "jupyter"))

(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "default/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t))))

(load! "+pretty")
(load! "+timestamp")

(dolist (flag doom--current-flags)
  (load! (symbol-name flag)))
