(defun +gptel-prepare-message ()
  (let ((template)
        (diff-str))
    (goto-char (point-min))
    (when (looking-at "#")
      (kill-line)
      (insert "# {[title of the pull request]}"))
    (setq template (buffer-string))
    (erase-buffer)
    (magit-fetch-all nil)
    (insert (format "\n\nCreating a pull request from %s to %s" forge--buffer-head-branch forge--buffer-base-branch))
    (magit-git-insert "log" "--oneline" "--no-merges" forge--buffer-head-branch (format "^%s" forge--buffer-base-branch))
    (insert "\n#+end_log\n")
    (insert "#+begin_diff\n")
    (magit-git-insert "diff" (format "%s...%s" forge--buffer-base-branch forge--buffer-head-branch))
    (insert "\n#+end_diff")
    (setq diff-str (buffer-string))
    (erase-buffer)
    (gptel-request (format
                    "#+begin_template\n%s\n#+end_template\n%s"
                    template
                    diff-str)
      :system (concat
               "Generate a pull request description with the given template, git log and git diff. "
               "DO NOT make up anything that does not exists in the log or diff. "
               "If git log and git diff is empty, tell the user the PR is not needed in the description.")
      :stream t)))

(setq forge-create-pullreq-hook
      '(+gptel-prepare-message))
