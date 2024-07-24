;; -*- lexical-binding: t; -*-

(use-package! gptel
  :config
  (setq! gptel-default-mode 'org-mode)
  (setq! gptel-org-branching-context 't)
  (require 'gptel-context)
  (evil-set-initial-state 'gptel-context-buffer-mode 'emacs)
  (map!
   :g "C-c C-g" #'gptel-send
   :g "C-c g" #'gptel-send
   (:leader
    (:prefix ("i" . "insert")
     :desc "generative AI" "g" #'gptel-send)
    (:prefix ("e" . "generate")
     :desc "send prompt" "e" #'gptel-send
     :desc "menu" "m" #'gptel-menu
     :desc "add context" "c" #'gptel-context-add
     :desc "examine context" "C" #'gptel--suffix-context-buffer))))

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

