(after! org
  (defun org-agenda-delete-empty-blocks ()
    "Remove empty agenda blocks.
  A block is identified as empty if there are fewer than 2
  non-empty lines in the block (excluding the line with
  `org-agenda-block-separator' characters)."
    (when org-agenda-compact-blocks
      (user-error "Cannot delete empty compact blocks"))
    (setq buffer-read-only nil)
    (save-excursion
      (goto-char (point-min))
      (let* ((blank-line-re "^\\s-*$")
             (content-line-count (if (looking-at-p blank-line-re) 0 1))
             (start-pos (point))
             (block-re (format "%c\\{10,\\}" org-agenda-block-separator)))
        (while (and (not (eobp)) (forward-line))
          (cond
           ((looking-at-p block-re)
            (when (< content-line-count 2)
              (delete-region start-pos (1+ (point-at-bol))))
            (setq start-pos (point))
            (forward-line)
            (setq content-line-count (if (looking-at-p blank-line-re) 0 1)))
           ((not (looking-at-p blank-line-re))
            (setq content-line-count (1+ content-line-count)))))
        (when (< content-line-count 2)
          (delete-region start-pos (point-max)))
        (goto-char (point-min))
        ;; The above strategy can leave a separator line at the beginning
        ;; of the buffer.
        (when (looking-at-p block-re)
          (delete-region (point) (1+ (point-at-eol))))))
    (setq buffer-read-only t))

  (add-hook! org-agenda-finalize #'org-agenda-delete-empty-blocks)
  (defun org-agenda-update (&rest _)
    (setq org-agenda-files '("~/org")))
  (advice-add 'org-agenda :before #'org-agenda-update)
  (advice-add 'org-todo-list :before #'org-agenda-update)

  (face-spec-set 'org-agenda-dimmed-todo-face
                 '((((background light)) (:foreground "DarkRed"))
                   (((background dark)) (:foreground "DarkRed"))))
  (setq org-agenda-start-with-follow-mode t)
  (map!
   :after evil-org-agenda
   :mode evil-org-agenda-mode
   :m "i"       #'org-agenda-clock-in
   :m "o"       #'org-agenda-clock-out
   :m "I"       #'org-agenda-diary-entry
   :m "O"       #'delete-other-windows
   :m [tab]      #'org-agenda-show-and-scroll-up
   :m [return]   #'+org-agenda-open-project
   :m [M-return] #'org-agenda-switch-to)
  ;; (map!
  ;;  :map org-agenda-mode-map
  ;;  :g "<tab>"      #'org-agenda-show-and-scroll-up
  ;;  :g "<return>"   #'org-agenda-goto
  ;;  :g "M-<return>" #'org-agenda-switch-to)

  (add-hook! 'org-agenda-after-show-hook #'org-narrow-to-subtree))
