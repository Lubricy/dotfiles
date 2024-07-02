(use-package! k8s-mode
  :after yaml
  :hook (k8s-mode . yas-minor-mode)
  :config
  (add-hook!
   'yaml-mode-hook
   (defun lubricy/enable-k8s-mode ()
     (save-match-data
       (let* ((path-before-template (when (string-match "\\(.*\\)/templates/.*" (buffer-file-name))
                                      (match-string 1 (buffer-file-name)))))
         (when (and
                (not (derived-mode-p 'k8s-mode))
                path-before-template
                (file-exists-p (concat (file-name-as-directory path-before-template) "/Chart.yaml")))
           (apheleia-mode -1)
           (k8s-mode))))))
  (require 'lsp-mode)
  (lsp-register-client (make-lsp-client
                        :new-connection (lsp-stdio-connection '("helm_ls" "serve"))
                        :activation-fn (lsp-activate-on "helm")
                        :server-id 'helm
                        :download-server-fn (lambda (_client callback error-callback _update?)
                                              (lsp-package-ensure 'yaml-language-server
                                                                  callback error-callback))))
  (add-to-list 'lsp-language-id-configuration '(k8s-mode . "helm")))
