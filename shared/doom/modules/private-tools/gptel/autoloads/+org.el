;; -*- lexical-binding: t; -*-

;;;autoload
(defun org-chatcompletion-data ()
  "Convert the nearest L1 heading before the point in the Org-mode buffer into
   OpenAI ChatCompletion data compatible with `json-encode`."
  (save-excursion
    (let ((l1-pos (find-nearest-l1-heading)))
      (when l1-pos
        (goto-char l1-pos)
        (parse-org-heading)))))

;;;###autoload
(defun parse-org-heading ()
  "Parse the current L1 heading in the Org-mode buffer and return a list of
  messages."
  ;; (org-mode 't)
  (mapcar (lambda (message)
            `((:role . ,(car message))
              (:content . ,(string-trim (cdr message)))))
          (org-element-to-chat-messages)))

;;;###autoload
(defun org-element-to-chat-messages ()
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let ((beg (point))
          (end (progn (org-end-of-subtree) (point))))
      (save-restriction
        (narrow-to-region beg end)
        (let* ((parsed-elements (org-element-parse-buffer))
               (headlines (org-element-map parsed-elements 'headline 'identity))
               (l2-headlines (cl-remove-if (lambda (headline) (/= 2 (org-element-property :level headline))) headlines))
               (conversation-text (org-element-map parsed-elements 'paragraph
                                    (lambda (paragraph)
                                      (when (eq (org-element-type (org-element-property :parent paragraph)) 'section)
                                        (buffer-substring (org-element-property :begin paragraph)
                                                          (org-element-property :end paragraph))))
                                    nil t))
               (messages (when conversation-text
                           (list (cons "system" (string-trim conversation-text))))))
          (setq messages (append messages
                                 (mapcar (lambda (headline)
                                           (let* ((title (org-element-interpret-data (org-element-property :title headline)))
                                                  (body (when (org-element-property :contents-begin headline)
                                                          (buffer-substring-no-properties (org-element-property :contents-begin headline)
                                                                                          (org-element-property :contents-end headline))))
                                                  (prefixes '("User:" "System:" "Assistant:"))
                                                  (message-type (cond
                                                                 ((string-match-p "^System" title) "system")
                                                                 ((string-match-p "^User" title) "user")
                                                                 ((string-match-p "^Assistant" title) "assistant")
                                                                 (t "user")))
                                                  (message-text (concat title (when body (concat "\n" (string-trim
                                                                                                       (if (string= message-type "assistant")
                                                                                                           body
                                                                                                         (process-org-links body)))))))
                                                  (cleaned-text (remove-prefixes message-text prefixes))
                                                  )
                                             (cons message-type cleaned-text)))
                                         l2-headlines)))
          messages)))))

;;;###autoload
(defun +gptel-org-send (&optonal arg)
  (interactive "P")
  (let ((gptel-pre-response-hook (lambda ()
                                   (org-insert-heading)
                                   (insert "Assistant:\n")))
        (prompt (org-chatcompletion-data)))
    (gptel-request prompt
      :)))
