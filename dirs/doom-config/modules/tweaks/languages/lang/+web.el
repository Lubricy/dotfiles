(after! web
  ;; node style compilation error
  (add-to-list
   'compilation-error-regexp-alist-alist
   '(node
     "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
     1 ;; file
     2 ;; line
     3 ;; column
     ))
  (add-to-list 'compilation-error-regexp-alist 'node))
