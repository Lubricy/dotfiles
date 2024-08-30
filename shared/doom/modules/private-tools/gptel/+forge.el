(defun +gptel-prepare-message ()
  (let ((template (buffer-string))
        (diff-str))
    (erase-buffer)
    (insert "#+begin_diff\n")
    (magit-git-insert "diff" (format "%s...%s" forge--buffer-base-branch forge--buffer-head-branch))
    (insert "\n#+end_diff")
    (setq diff-str (buffer-string))
    (erase-buffer)
    (gptel-request (format
                    "#+begin_template\n%s\n#+end_template\n%s"
                    template
                    diff-str)
      :system "generate a pull request description with the given template and diff."
      :stream t)))

(setq forge-create-pullreq-hook
      '(+gptel-prepare-message))
