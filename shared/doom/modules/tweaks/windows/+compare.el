(defun rtc-comp2 ()
  (cl-destructuring-bind
      (f1 f2 n1 n2) server-eval-args-left
    (ediff
     f1 f2
     (list
      (lambda ()
        (with-current-buffer ediff-buffer-A
          (rename-buffer (format "new/%s" n1)))
        (with-current-buffer ediff-buffer-B
          (rename-buffer (format "old/%s" n2))))))
    (setq server-eval-args-left nil)))

(defun rtc-comp3 ()
  (let ((ediff-show-ancestor nil)
        (args server-eval-args-left))
    (setq server-eval-args-left nil)
    (cl-destructuring-bind
        (f1 f2 f3 fr n1 n2 n3 nr) args
      (ediff-merge-files-with-ancestor
       f1 f2 f3
       (list
        (lambda ()
          (with-current-buffer ediff-buffer-A
            (setq buffer-file-name nil)
            ;; (rename-buffer (format "new/%s" n1))
            (read-only-mode 1))
          (with-current-buffer ediff-buffer-B
            (setq buffer-file-name nil)
            ;; (rename-buffer (format "old/%s" n2))
            (read-only-mode 1))
          (when ediff-show-ancestor
            (ediff-toggle-show-ancestor))))
       fr))))
