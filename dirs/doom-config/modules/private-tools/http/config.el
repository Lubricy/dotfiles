;;; tools/gnuplot/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(use-package! ob-http
  :after org
  :config
  (defadvice! lubricy/org-babel-execute:http (body params)
    :override 'org-babel-execute:http
    (let* ((request (ob-http-parse-request (org-babel-expand-body:http body params)))
           (proxy (cdr (assoc :proxy params)))
           (noproxy (assoc :noproxy params))
           (follow-redirect (and (assoc :follow-redirect params) (not (string= "no" (cdr (assoc :follow-redirect params))))))
           (pretty (assoc :pretty params))
           (prettify (and pretty (not (string= (cdr pretty) "no"))))
           (file (assoc :file params))
           (get-header (cdr (assoc :get-header params)))
           (cookie-jar (cdr (assoc :cookie-jar params)))
           (cookie (cdr (assoc :cookie params)))
           (curl (cdr (assoc :curl params)))
           (select (cdr (assoc :select params)))
           (resolve (cdr (assoc :resolve params)))
           (request-body (ob-http-request-body request))
           (error-output (org-babel-temp-file "curl-error"))
           (args (append ob-http:curl-custom-arguments
                         (list "-i"
                               (when (and proxy (not noproxy)) `("-x" ,proxy))
                               (when noproxy '("--noproxy" "*"))
                               (let ((method (ob-http-request-method request)))
                                 (if (string= "HEAD" method) "-I" `("-X" ,method)))
                               (when follow-redirect "-L")
                               (when (and (assoc :username params) (assoc :password params))
                                 `("--user" ,(s-format "${:username}:${:password}" 'ob-http-aget params)))
                               (when (assoc :user params) `("--user" ,(cdr (assoc :user params))))
                               (mapcar (lambda (x) `("-H" ,x)) (ob-http-request-headers request))
                               (when (s-present? request-body)
                                 (let ((tmp (org-babel-temp-file "http-")))
                                   (with-temp-file tmp (insert request-body))
                                   `("--data-binary" ,(format "@%s" tmp))))
                               (when cookie-jar `("--cookie-jar" ,cookie-jar))
                               (when cookie `("--cookie" ,cookie))
                               (when resolve (mapcar (lambda (x) `("--resolve" ,x)) (split-string resolve ",")))
                               (when curl (split-string-and-unquote curl))
                               "--max-time"
                               (int-to-string (or (cdr (assoc :max-time params))
                                                  ob-http:max-time))
                               "--globoff"
                               (ob-http-construct-url (ob-http-request-url request) params)))))
      (with-current-buffer (get-buffer-create "*curl commands history*")
        (goto-char (point-max))
        (insert "curl "
                (string-join (mapcar 'shell-quote-argument (ob-http-flatten args)) " ")
                "\n"))
      (with-current-buffer (get-buffer-create "*curl output*")
        (erase-buffer)
        (if (= 0 (apply 'call-process "curl" nil `(t ,error-output) nil (ob-http-flatten args)))
            (let ((response (ob-http-parse-response (buffer-string))))
              (when prettify (ob-http-pretty-response response (cdr pretty)))
              (when ob-http:remove-cr (ob-http-remove-carriage-return response))
              (cond (get-header (ob-http-get-response-header response get-header))
                    (select (ob-http-select response select))
                    (prettify (ob-http-response-body response))
                    (file (ob-http-file response (cdr file)))
                    (t (s-join "\n\n" (list (ob-http-response-headers response) (ob-http-response-body response))))))
          (with-output-to-temp-buffer "*curl error*"
            (princ (with-temp-buffer
                     (insert-file-contents-literally error-output)
                     (s-join "\n" (s-lines (buffer-string)))))
            "")))))
  )

;; cache candidates for better performance
