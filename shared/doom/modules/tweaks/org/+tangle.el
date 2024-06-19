(after! ob-tangle
  (defun +org-babel-tangle-maybe-delete-file (filename &optional trash)
    "Conditional delete file base on org babel header args."
    ;;; TODO fix me
    (let ((tangle-write (funcall get-spec :tangle-write))
          (tangle-append (funcall get-spec :tangle-append)))
      (unless (or (and tangle-write (string= tangle-write "append"))
                  (and tangle-append (or (string= tangle-append "yes")
                                         (string= tangle-append "t"))))
        (delete-file filename trash))))

  (defun org-babel-tangle-a (orig-fn &rest args)
    "Append source code block at point to its tangle file.
     The command works like `org-babel-tangle'
     but `delete-file' is ignored when `:tangle-append' is `append'."
    (cl-letf (((symbol-function 'delete-file) #'+org-babel-tangle-maybe-delete-file))
      (apply orig-fn args)))

  (advice-add #'org-babel-tangle :around #'org-babel-tangle-a))
