;;; -*- lexical-binding: t; -*-

(require 'json)
(require 'org-element)
(require 'request)

;; Generate a unique hash to be used as a placeholder.
;;;###autoload
(defun generate-unique-hash ()
  "Generate a unique hash to be used as a placeholder."
  (format "<#%s#>" (md5 (format "%s%s" (current-time) (random 1000000)))))

;;;###autoload
(defun remove-prefixes (str prefixes)
  (if (null prefixes)
      str
    (remove-prefixes (replace-regexp-in-string (car prefixes) "" str) (cdr prefixes))))

;;;###autoload
(defun pandoc-html-to-text (html)
  (with-temp-buffer
    (insert html)
    (shell-command-on-region (point-min) (point-max) "pandoc -f html -t plain" nil t)
    (buffer-string)))

;;;###autoload
(defun process-org-links (text)
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward org-link-bracket-re nil t)
      (let* ((match-data-list (match-data))
             (link (if (>= (length match-data-list) 2) (match-string-no-properties 1) ""))
             (desc (if (>= (length match-data-list) 4) (match-string-no-properties 3) link)))
        (cond
         ((string-match-p "^file:" link)
          (let* ((unescaped-link (replace-regexp-in-string "^file:" "" link))
                 (expanded-link (expand-file-name unescaped-link)))
            (when (file-exists-p expanded-link)
              (let ((file-name (file-name-nondirectory expanded-link))
                    (file-contents (with-temp-buffer (insert-file-contents expanded-link) (buffer-string))))
                (when (not (string-empty-p file-contents))
                  (replace-match
                   (concat "\n\n# Filename: " file-name "\n```\n" file-contents "```\n\n")
                   t t))))))
         ((string-match-p "^http" link)
          (let ((url-content (save-match-data (request-response-data (request link :sync t)))))
            (when (not (string-empty-p url-content))
              (replace-match (format "\n\n# URL: %s\n```\n%s\n```\n\n" link (pandoc-html-to-text url-content)) t t)))))))
    (buffer-string)))


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
(defun parse-org-heading ()
  "Parse the current L1 heading in the Org-mode buffer and return a list of
  messages."
  ;; (org-mode 't)
  (mapcar (lambda (message)
            `((:role . ,(car message))
              (:content . ,(string-trim (cdr message)))))
          (org-element-to-chat-messages)))

;;;###autoload
(defun find-nearest-l1-heading ()
  "Find the nearest L1 heading before the point."
  (save-excursion
    (while (and (not (bobp)) (not (= (org-current-level) 1)))
      (org-previous-visible-heading 1))
    (when (= (org-current-level) 1)
      (point))))

;;;###autoload
(defun org-chatcompletion-data ()
  "Convert the nearest L1 heading before the point in the Org-mode buffer into
   OpenAI ChatCompletion data compatible with `json-encode`."
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

;;;###autoload
(defun org-babel-openai-get-api-key ()
  (unless (boundp 'org-babel-api-key-expiration-time)
    (setq org-babel-api-key-expiration-time 0))
  (if (s-equals? org-babel-openai-auth "aad")
      (let ((expired (> (time-convert nil 'integer) org-babel-api-key-expiration-time)))
        (if expired
            (let ((resp (request-response-data
                         (request "https://login.microsoftonline.com/fmronline.onmicrosoft.com/oauth2/v2.0/token"
                           :type "POST"
                           :data `(("client_id" . ,org-babel-openai-auth-aad-client-id)
                                   ("scope" . "https://cognitiveservices.azure.com/.default")
                                   ("username" . ,org-babel-openai-auth-username)
                                   ("password" .  ,(if (string-empty-p org-babel-openai-auth-password)
                                                       (password-store-get org-babel-openai-auth-password-entry)
                                                     org-babel-openai-auth-password))
                                   ("grant_type" . "password"))
                           :parser 'json-read
                           :sync t))))
              (setq org-babel-api-key-expiration-time (+ (time-convert nil 'integer) (alist-get 'expires_in resp)))
              (setq org-babel-openai-api-key (alist-get 'access_token resp)))
          org-babel-openai-api-key))
    org-babel-openai-api-key))


;;;###autoload
(defun openai-chatcompletion-send-request (messages &optional callback)
  "Send a request to the OpenAI ChatCompletion endpoint with the given MESSAGES.

The CALLBACK function, if provided, will be called with the assistant's message
as a string when the request completes."
  (let* ((provider org-babel-openai-default-provider)
         (api-key (org-babel-openai-get-api-key))
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
      :timeout 3600
      :headers `(("Content-Type" . "application/json")
                 ,(if (string= org-babel-openai-auth "aad")
                      `("Authorization" . ,(format "Bearer %s" api-key))
                    `("api-key" . ,api-key)))
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


;;;###autoload
(defun insert-openai-chatcompletion-response ()
  "Insert the OpenAI ChatCompletion response at the correct place in the current
   Org file."
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
         (let ((formatted-response (org-openai-format-response response)))
           (save-excursion
             (with-current-buffer buffer
               (goto-char (point-min))
               (when (search-forward placeholder nil t)
                 (replace-match formatted-response t t))))))))))

;;;###autoload
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
