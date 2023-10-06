;;; -*- lexical-binding: t; -*-

(require 'json)
(require 'org-element)
(require 'request)

;; Generate a unique hash to be used as a placeholder.
(defun generate-unique-hash ()
  "Generate a unique hash to be used as a placeholder."
  (format "<#%s#>" (md5 (format "%s%s" (current-time) (random 1000000)))))

(defun remove-prefixes (str prefixes)
  (if (null prefixes)
      str
    (remove-prefixes (replace-regexp-in-string (car prefixes) "" str) (cdr prefixes))))

(defun process-org-links (text)
  (with-temp-buffer
     (insert text)
     (goto-char (point-min))
     (while (re-search-forward org-bracket-link-regexp nil t)
       (let* ((link (match-string-no-properties 1))
              (desc (or (match-string-no-properties 3) (match-string-no-properties 1)))
              (unescaped-link (replace-regexp-in-string "^file:" "" link))
              (expanded-link (expand-file-name unescaped-link))
              (file-name (if (file-exists-p expanded-link) (file-name-nondirectory expanded-link) ""))
              (file-contents (if (file-exists-p expanded-link)
                                 (with-temp-buffer (insert-file-contents expanded-link) (buffer-string))
                               (if (string-match-p "^http" link)
                                   (ignore-errors (with-current-buffer (url-retrieve-synchronously link) (buffer-string)))))))
         (replace-match (if file-contents
                            (concat "\n\n# Filename: " file-name "\n```\n" file-contents "```\n\n")
                          (or desc link)) t t)))
     (buffer-string)))

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

(defun parse-org-heading ()
  "Parse the current L1 heading in the Org-mode buffer and return a list of messages."
  (org-mode)
  (mapcar (lambda (message)
            `((:role . ,(car message))
              (:content . ,(string-trim (cdr message)))))
          (org-element-to-chat-messages)))

(defun find-nearest-l1-heading ()
  "Find the nearest L1 heading before the point."
  (save-excursion
    (while (and (not (bobp)) (not (= (org-current-level) 1)))
      (org-previous-visible-heading 1))
    (when (= (org-current-level) 1)
      (point))))

(defun org-chatcompletion-data ()
  "Convert the nearest L1 heading before the point in the Org-mode buffer into OpenAI ChatCompletion data compatible with `json-encode`."
  (save-excursion
    (let ((l1-pos (find-nearest-l1-heading)))
      (when l1-pos
        (goto-char l1-pos)
        (parse-org-heading)))))

(define-minor-mode org-chatcompletion-mode
  "A minor mode to convert Org-mode L1 headings into OpenAI ChatCompletion data."
  :lighter " Org-CC"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-x C-o") 'org-chatcompletion-data)
            map)
  (if org-chatcompletion-mode
      (unless (eq major-mode 'org-mode)
        (setq org-chatcompletion-mode nil)
        (user-error "org-chatcompletion-mode can only be enabled in Org-mode buffers"))))


(defun openai-chatcompletion-send-request (messages &optional callback)
  "Send a request to the OpenAI ChatCompletion endpoint with the given MESSAGES.

The CALLBACK function, if provided, will be called with the assistant's message
as a string when the request completes."
  (let* ((provider org-babel-openai-default-provider)
         (api-key org-babel-openai-api-key)
         (api-url org-babel-openai-api-url)
         (api-version (if (equal provider "openai")
                          org-babel-openai-default-api-version-openai
                        org-babel-openai-default-api-version-azure))
         (model org-babel-openai-default-model)
         (deployment (if (equal provider "openai")
                         (concat "v1/chat/completions")
                       (concat "openai/deployments/" model "/chat/completions")))
         (max-tokens org-babel-openai-default-max-tokens)
         (temperature org-babel-openai-default-temperature)
         (frequency-penalty org-babel-openai-default-frequency-penalty)
         (presence-penalty org-babel-openai-default-presence-penalty)
         (top-p org-babel-openai-default-top-p)
         (stop org-babel-openai-default-stop)
         (request-url (concat api-url "/" deployment "?api-version=" api-version)))
    (request request-url
      :type "POST"
      :headers `(("Content-Type" . "application/json")
                 ("api-key" . ,api-key))
      :data (json-encode `(("messages" . ,messages)
                           ("max_tokens" . ,max-tokens)
                           ("temperature" . ,temperature)
                           ("frequency_penalty" . ,frequency-penalty)
                           ("presence_penalty" . ,presence-penalty)
                           ("top_p" . ,top-p)
                           ("stop" . ,stop)))
      :parser 'json-read
      :error (cl-function (lambda (&rest args &key data error-thrown &allow-other-keys)
                            (let ((error-message (format "Got error: %S %S" error-thrown data)))
                              (message error-message)
                              (when callback
                                (funcall callback error-message)))))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let ((assistant-message (alist-get 'content (alist-get 'message (elt (alist-get 'choices data) 0)))))
                    (when callback
                      (funcall callback assistant-message))))))))


(defun insert-openai-chatcompletion-response ()
  "Insert the OpenAI ChatCompletion response at the correct place in the current Org file."
  (interactive)
  (let ((messages (org-chatcompletion-data))
        (placeholder (generate-unique-hash))
        (buffer (current-buffer)))
    (when messages
      (save-excursion
        (org-insert-heading 2)
        (insert "Assistant:\n")
        (insert placeholder))
      (openai-chatcompletion-send-request
       messages
       (lambda (response)
         (let ((formated-response (org-openai-format-response response)))
           (save-excursion
             (with-current-buffer buffer
               (goto-char (point-min))
               (when (search-forward placeholder nil t)
                 (replace-match formated-response t t))))))))))

(defun org-openai-format-response (markdown)
  "Transform a Markdown text into an Org-mode text.
Replace Markdown code blocks with Org-mode source blocks.
If no language is specified, use example blocks instead."
  (with-temp-buffer
    (insert markdown)
    (goto-char (point-min))
    (while (re-search-forward "^```\\(.*\\)$" nil t)
      (let ((lang (match-string 1)))
        (replace-match (if (string-empty-p lang)
                           "#+begin_example"
                         (concat "#+begin_src " lang)))
        (when (re-search-forward "^```$" nil t)
          (replace-match (if (string-empty-p lang)
                             "#+end_example"
                           "#+end_src")))))
    (buffer-string)))

