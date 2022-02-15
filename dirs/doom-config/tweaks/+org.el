(defun lubricy/babel-ansi ()
  (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (ansi-color-context-region nil))
          (ansi-color-apply-on-region beg end))))))

(defun shk-fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(defun lubricy/org-babel-node-setenv (&optional root)
  (interactive "DProject root:")
  (let ((root (or root (projectile-locate-dominating-file default-directory "package.json"))))
    (when root
      (let* ((node-modules (concat (file-name-as-directory root) "node_modules"))
             (node-path (getenv "NODE_PATH"))
             (node-paths (if node-path (split-string node-path ":") '())))
        (setenv "NODE_PATH" (string-join (delete-dups (cons node-modules node-paths)) ":"))))))

(defun lubricy/test-echo ()
  (message "hook!"))

(after! org
  (setq org-agenda-search-view-always-boolean t
        org-fancy-priorities-list '("" "" ""))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "|" "DONE(d)")
          (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CNCL(x@/!)" "TRASH(s)" "CALL(c)" "MEET(m)" "REST(r)"))
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
  (add-hook! org-mode #'lubricy/org-babel-node-setenv)
  (add-hook! org-mode #'+org-pretty-mode)
  (add-hook! org-babel-after-execute #'lubricy/babel-ansi)
  (add-hook! org-babel-after-execute #'shk-fix-inline-images)
  (add-hook! ob-async-pre-execute-src-block
    (load (expand-file-name "init.el" user-emacs-directory)))
  (defadvice! org-babel-record-timestamp (orig-fn &rest args)
    :around 'org-babel-execute-src-block
    (let* ((info (or (nth 1 args) (org-babel-get-src-block-info)))
           (begin-prompt "#+S_TIME: ")
           (end-prompt   "#+E_TIME: ")
           (ts-format   "[%F %T.%6N %z]")
           (code-block-params (or (nth 2 args) (nth 2 info)))
           (code-block-name (nth 4 info))
           (block-start (nth 5 info))
           (timestamp (cdr (assoc :timestamp code-block-params)))
           (cache (let ((c (cdr (assq :cache code-block-params))) )
                    (and (not (car args)) c (string= "yes" c))))
           (new-hash (and cache (org-babel-sha1-hash info)))
           (old-hash (and cache (org-babel-current-result-hash)))
           (current-cache (and new-hash (equal new-hash old-hash))))
      (if (and (equal timestamp "t")
               (not current-cache))
          (let ((begin-time (format-time-string ts-format)))
            (apply orig-fn args)
            (let ((end-time (format-time-string ts-format)))
              (save-excursion
                (goto-char block-start)
                (when (> (length code-block-name) 0)
                  (search-backward code-block-name))
                (forward-line -1)
                (beginning-of-line)
                (if (search-forward end-prompt (line-end-position) t)
                    (progn
                      (beginning-of-line)
                      (kill-line))
                  (progn
                    (end-of-line)
                    (insert "\n")))
                (insert (concat end-prompt end-time))
                (forward-line -1)
                (beginning-of-line)
                (if (search-forward begin-prompt (line-end-position) t)
                    (progn
                      (beginning-of-line)
                      (kill-line))
                  (progn
                    (end-of-line)
                    (insert "\n")))
                (insert (concat begin-prompt begin-time))
                )))
        (apply orig-fn args)))))

(after! ob-async
  (pushnew! ob-async-no-async-languages-alist "jupyter"))

(after! org-roam
  (setq org-roam-capture-templates
    '(("d" "default" plain "%?"
       :target (file+head "default/%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+title: ${title}\n")
       :unnarrowed t))))
