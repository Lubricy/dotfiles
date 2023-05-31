;;; -*- lexical-binding: t; -*-

(defcustom org-babel-openai-api-key ""
  "Default API key for OpenAI."
  :type 'string
  :group 'org-babel)

(defcustom org-babel-openai-default-model "gpt-4-32k"
  "Default model for OpenAI."
  :type 'string
  :group 'org-babel)

(defcustom org-babel-openai-default-api-version-azure "2023-03-15-preview"
  "Default API version for OpenAI with the Azure provider."
  :type 'string
  :group 'org-babel)

(defcustom org-babel-openai-default-api-version-openai "v1"
  "Default API version for OpenAI with the OpenAI provider."
  :type 'string
  :group 'org-babel)

(defcustom org-babel-openai-default-provider "openai"
  "Default provider for OpenAI ('azure' or 'openai')."
  :type 'string
  :group 'org-babel)

(defcustom org-babel-openai-api-url "https://api.openai.com"
  "Default base API URL for OpenAI with the OpenAI provider."
  :type 'string
  :group 'org-babel)


(defcustom org-babel-openai-default-max-tokens 800
  "Default max tokens for OpenAI."
  :type 'integer
  :group 'org-babel)

(defcustom org-babel-openai-default-temperature 0.7
  "Default temperature for OpenAI."
  :type 'float
  :group 'org-babel)

(defcustom org-babel-openai-default-frequency-penalty 0
  "Default frequency penalty for OpenAI."
  :type 'integer
  :group 'org-babel)

(defcustom org-babel-openai-default-presence-penalty 0.
  "Default presence penalty for OpenAI."
  :type 'float
  :group 'org-babel)

(defcustom org-babel-openai-default-top-p 0.95
  "Default top_p for OpenAI."
  :type 'float
  :group 'org-babel)

(defcustom org-babel-openai-default-stop nil
  "Default stop for OpenAI."
  :type 'string
  :group 'org-babel)

(defun openai-format-request (text)
  "Convert plain text to OpenAI request format."
  (let ((lines (split-string text "\n")))
    (mapcar (lambda (line)
              (let* ((parts (split-string line ": "))
                     (role (car parts))
                     (content (cadr parts)))
                `((role . ,role) (content . ,content))))
            lines)))

(defun openai-format-response (response)
  "Convert OpenAI response format to plain text."
  (let ((choices (alist-get 'choices response)))
    (when (> (length choices) 0)
      (let ((message (aref choices 0)))
        (concat (alist-get 'role (alist-get 'message message)) ": "
                (alist-get 'content (alist-get 'message message)))))))


(defun org-babel-execute:openai (body params)
  "Execute an OpenAI code block in org-babel."
  (let* ((provider (or (cdr (assq :provider params))
                       org-babel-openai-default-provider))
         (api-key (or (cdr (assq :api_key params))
                      org-babel-openai-api-key))
         (api-url (or (cdr (assq :api_url params))
                      org-babel-openai-api-url))
         (api-version (or (cdr (assq :api_version params))
                          (if (equal provider "openai")
                              org-babel-openai-default-api-version-openai
                            org-babel-openai-default-api-version-azure)))
         (model (or (cdr (assq :model params))
                    org-babel-openai-default-model))
         (deployment (if (equal provider "openai")
                         (concat "v1/engines/" model "/completions")
                       (concat "openai/deployments/" model "/chat/completions")))
         (max-tokens (or (cdr (assq :max_tokens params))
                         org-babel-openai-default-max-tokens))
         (temperature (or (cdr (assq :temperature params))
                          org-babel-openai-default-temperature))
         (frequency-penalty (or (cdr (assq :frequency_penalty params))
                                org-babel-openai-default-frequency-penalty))
         (presence-penalty (or (cdr (assq :presence_penalty params))
                               org-babel-openai-default-presence-penalty))
         (top-p (or (cdr (assq :top_p params))
                    org-babel-openai-default-top-p))
         (stop (or (cdr (assq :stop params))
                   org-babel-openai-default-stop))
         (messages (openai-format-request body))
         (request-url (concat api-url "/" deployment "?api-version=" api-version)))

    (openai-format-response (request-response-data (request request-url
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
                             :sync t)))))
(load! "+org")

(after! org
  (add-hook! org-mode
    (org-chatcompletion-mode 1))
  (map!
   :localleader
   (:map org-chatcompletion-mode-map
    :desc "chat complete" "z" #'insert-openai-chatcompletion-response)))
