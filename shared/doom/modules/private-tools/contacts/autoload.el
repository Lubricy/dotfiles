;; -*- lexical-binding: t; -*-

;;;###autoload
(defun contact-which (uid)
  (gethash uid contact-repo))

;;;###autoload
(defsubst contact--identity-provider ()
  (alist-get contact-default-identity-provider
             contact-identity-providers-alist))

;;;###autoload
(defsubst contact-formatter (contact)
  (funcall (identity-provider-formatter
            (contact--identity-provider))
           (if (contact-p contact)
               (contact-props contact)
             contact)))

;;;###autoload
(defsubst contact-formats ()
  (identity-provider-formats
   (contact--identity-provider)))

;;;###autoload
(defsubst contact/org-tags ()
  (identity-provider-org
   (contact--identity-provider)))

;;;###autoload
(defun contact/format (format contact)
  (let ((spec (contact-formatter contact))
        (fstring (if (stringp format)
                     format
                   (or (alist-get format (contact-formats) nil nil #'equal)
                       (error "Invalid format `%s`" format)))))
    (format-spec fstring spec)))

;;;###autoload
(defun contact--representation (contact)
  (contact/format '--representation contact))

;;;###autoload
(defun contact-fetch (query)
  (let ((candidates '()))
    (when query
      (dolist (contact (funcall (identity-provider-fetcher
                                 (contact--identity-provider))
                                query))
        (let* ((candidate (contact--create
                           :uid (contact/format 'uid contact)
                           :props contact))
               (uid (contact-uid candidate)))
          (puthash uid candidate contact-repo)
          (cl-pushnew uid candidates))))
    candidates))

;;;###autoload
(defun contact-all-formats (candidate &optional flag)
  (cl-loop for (k . v) in (contact-formats)
           when (or flag (not (s-starts-with? "-" (symbol-name k))))
           collect (contact/format v (if (stringp candidate)
                                         (contact-which candidate)
                                       candidate))))

;;;###autoload
(defun contact-find--cache (&optional pred)
  (if pred
      (cl-loop for k being the hash-keys of contact-repo
               using (hash-values v)
               when (funcall pred v)
               collect k)
    (hash-table-keys contact-repo)))

;;;###autoload
(defun contact-find-cache (query)
  (if query
      (contact-find--cache (lambda (v) (string-match query (contact--representation v))))
    (contact-find--cache)))

;;;###autoload
(defun contact--completing-fn (query pred flag)
  (when (bound-and-true-p contact--debounce-timer)
    (cancel-timer contact--debounce-timer))
  (let ((candidates (contact-find--cache
                     (lambda (v)
                       (let ((representation (contact--representation v)))
                         (and (--all?
                               (string-match it representation)
                               completion-regexp-list)
                              (string-match query representation)
                              (if pred (pred representation) t)))))))
    (pcase flag
      ('t ;; all-completions
       (unless (or candidates (string-empty-p query))
         (setq contact--debounce-timer
               (run-with-idle-timer
                2 nil
                #'contact-fetch
                query)))
       candidates))))

;;;###autoload
(defun contact/find (&optional query)
  (pcase (or (contact-find-cache query) (contact-fetch query))
    (`(,single) single)
    (_
     (completing-read
      "Find Contact: " #'contact--completing-fn
      nil nil query))))

;;;###autoload
(defun contact/insert (&optional query)
  (interactive)
  (thread-last
    query
    (contact/find)
    (contact-all-formats)
    (completing-read "Which Contact Format: ")
    (insert)))
