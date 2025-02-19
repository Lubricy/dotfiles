;; -*- lexical-binding: t; -*-

;;;###autoload
(defun +age-github-keys-for (username)
  "Turn GitHub USERNAME into a list of ssh public keys."
  (let* ((res (shell-command-to-string
               (format "curl -s https://api.github.com/users/%s/keys"
                       (shell-quote-argument username))))
         (json (json-parse-string res :object-type 'alist)))
    (cl-assert (arrayp json))
    (cl-loop for alist across json
             for key = (cdr (assoc 'key alist))
             when (and (stringp key)
                       (string-match-p "^ssh-ed25519" key))
             collect key)))

