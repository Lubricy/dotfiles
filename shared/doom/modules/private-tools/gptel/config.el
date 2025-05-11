;; -*- lexical-binding: t; -*-

(use-package! gptel
  :config
  (setq! gptel-default-mode 'org-mode)
  (setq! gptel-org-branching-context 't)
  (setq! gptel-directives
         '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely. I'm looking for best result possible. Before you give me the answer, ask me everything you need to know to give me the best result possible. Be sure to ask questions one at a time since this is an interactive session.")
           (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
           (writing . "You are a large language model and a writing assistant. Respond concisely.")
           (chat . "You are a large language model and a conversation partner. Respond concisely. I'm looking for best result possible. Before you give me the answer, ask me everything you need to know to give me the best result possible. Be sure to ask questions one at a time since this is an interactive session.")))

  (require 'gptel-context)
  (evil-set-initial-state 'gptel-context-buffer-mode 'emacs)
  (require 'gptel-integrations)
  )

;;;###autoload
(defun +gptel-openai-get-api-key ()
  (unless (boundp '+gptel-api-key-expiration-time)
    (setq +gptel-api-key-expiration-time 0))
  (let ((expired (> (time-convert nil 'integer) +gptel-api-key-expiration-time)))
    (if expired
        (let ((resp (request-response-data
                     (request "https://login.microsoftonline.com/fmronline.onmicrosoft.com/oauth2/v2.0/token"
                       :type "POST"
                       :data `(("client_id" . ,+gptel-openai-auth-aad-client-id)
                               ("scope" . "https://cognitiveservices.azure.com/.default")
                               ("username" . ,+gptel-openai-auth-username)
                               ("password" .  ,(if (bound-and-true-p +gptel-openai-auth-password)
                                                   +gptel-openai-auth-password
                                                 (password-store-get +gptel-openai-auth-password-entry)))
                               ("grant_type" . "password"))
                       :parser 'json-read
                       :sync t))))
          (setq +gptel-api-key-expiration-time (+ (time-convert nil 'integer) (alist-get 'expires_in resp)))
          (setq +gptel-openai-api-key (alist-get 'access_token resp)))
      +gptel-openai-api-key))
  +gptel-openai-api-key)

(use-package! starhugger
  :after prog-mode
  :config
  (setq starhugger-completion-backend-function #'starhugger-ollama-auth-completion-api)
  (add-to-list 'global-mode-string '(:propertize (starhugger-auto-mode "")
                                     face doom-modeline-notification)))

(defcustom starhugger-ollama-additional-headers-alist
  '((options) (stream . :false))
  "Ollama API's advanced parameters.
See https://github.com/ollama/ollama/blob/main/docs/api.md#parameters."
  :group 'starhugger
  :type 'alist)

(cl-defun starhugger-ollama-auth-completion-api
    (prompt
     callback &rest args &key model force-new max-new-tokens &allow-other-keys)
  (-let* ((model (or model starhugger-model-id))
          (sending-data
           (starhugger--json-serialize
            `((prompt . ,prompt)
              (model . ,model)
              (options
               ,@(and max-new-tokens `((num_predict . ,max-new-tokens)))
               ,@(and force-new
                      starhugger-retry-temperature-range
                      `((temperature . ,(starhugger--retry-temperature))))
               ,@(and starhugger-chop-stop-token
                      `((stop . [,@starhugger-stop-tokens])))
               ,@(alist-get 'options starhugger-ollama-additional-parameter-alist))
              (stream . :false)
              ,@starhugger-ollama-additional-parameter-alist))))
    (starhugger--log-before-request
     starhugger-ollama-generate-api-url sending-data)
    (-let* ((request-obj
             (starhugger--request-dot-el-request
               starhugger-ollama-generate-api-url
               :type "POST"
               :headers starhugger-ollama-additional-headers-alist
               :data sending-data
               :error #'ignore
               :complete
               (cl-function
                (lambda (&rest
                         returned
                         &key
                         data
                         error-thrown
                         response
                         &allow-other-keys)
                  (-let* ((generated-lst
                           (if error-thrown
                               '()
                             (-some-->
                                 data
                               (json-parse-string it :object-type 'alist)
                               (list (alist-get 'response it))))))
                    (starhugger--log-after-request
                     (list
                      :response-content returned
                      :send-data sending-data
                      :response-status
                      (request-response-status-code response))
                     error-thrown)
                    (funcall callback
                             generated-lst
                             :model model
                             :error
                             (and error-thrown
                                  `((error-thrown ,error-thrown)
                                    (data ,data)))))))))
            (request-buf (request-response--buffer request-obj))
            (request-proc (get-buffer-process request-buf))
            (cancel-fn (lambda () (request-abort request-obj))))
      (list
       :cancel-fn cancel-fn
       :process request-proc
       :request-response request-obj))))

(load! "+forge.el")

(use-package! mcp-hub
  :after gptel
  :config
  (setq! mcp-hub-servers '(("fetch" . (:command "uvx" :args ("mcp-server-fetch"))))))

;;;###autoload
(defun gptel-mcp-register-tool ()
  (interactive)
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (apply #'gptel-make-tool
                       tool))
            tools)))
