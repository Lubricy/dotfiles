;;;###autoload
(defun lubricy/babel-ansi ()
  (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (ansi-color-context-region nil))
          (ansi-color-apply-on-region beg end))))))

;;;###autoload
(defun shk-fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

;;;###autoload
(defun lubricy/org-babel-node-setenv (&optional root)
  (interactive "DProject root:")
  (let ((root (or root (projectile-locate-dominating-file default-directory "package.json"))))
    (when root
      (let* ((node-modules (concat (file-name-as-directory root) "node_modules"))
             (node-path (getenv "NODE_PATH"))
             (node-paths (if node-path (split-string node-path ":") '())))
        (setenv "NODE_PATH" (string-join (delete-dups (cons node-modules node-paths)) ":"))))))
