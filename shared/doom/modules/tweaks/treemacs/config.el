;; treemacs to show git directory highlights

;;;###autoload
(defun +org/find-attached-file-default-directory ()
  "find the org file attached to this directory."
  (interactive)
  (let ((filename (when (s-ends-with? ".assets/" default-directory)
                    (string-remove-suffix ".assets/" default-directory)))
        (id (parse-uuid-from-file-directory)))
    (cond
     (filename (find-file filename))
     (id (org-roam-id-open id nil))
     (t (message "directory seems not attached to anywhere.")))))

;;;###autoload
(defun parse-uuid-from-file-directory (&optional dir)
  "parse an uuid string from directory."
  (let* ((dir (or dir default-directory))
         (sanitized (s-replace-regexp "[^-0-9a-f]" "" (downcase dir))))
    (save-match-data
      (when (string-match
             ;; taken from `org-uuidgen-p'
             "\\([0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\)"
             sanitized)
        (match-string 1 sanitized)))))

(after! treemacs
  (setq treemacs-collapse-dirs 20
        +treemacs-git-mode 'deferred)
  (map!
   :map evil-treemacs-state-map
   :g "a" #'+org/find-attached-file-default-directory)
  ;; HACK
  (evil-define-key* 'treemacs treemacs-mode-map (kbd "h") nil))

;; (after! doom-themes-ext-treemacs
;;   (with-eval-after-load 'treemacs
;;     ;; HACK: dim treemacs bg with solaire-mode
;;     (push '(treemacs-window-background-face . solaire-default-face) solaire-mode-remap-alist)
;;     (push '(treemacs-hl-line-face . solaire-hl-line-face) solaire-mode-remap-alist)
;;     ;; HACK: make dap breakpoint show on fringe
;;     (remove-hook 'treemacs-mode-hook #'doom-themes-hide-fringes-maybe)
;;     (advice-remove #'treemacs-select-window #'doom-themes-hide-fringes-maybe)
;;     ;; HACK: fix icon issue
;;     (treemacs-modify-theme "doom-atom" :config '())))
