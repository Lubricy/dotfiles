;;;###autoload
(defmacro load-directory! (dir &optional root)
  `(let* ((root (or ,root (dir!)))
          (path (expand-file-name ,dir root)))
     (mapc (lambda (file) (load! file)) (directory-files-recursively path "\\.el$"))))
