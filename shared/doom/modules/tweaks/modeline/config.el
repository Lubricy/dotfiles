(after! doom-modeline
  (defcustom doom-modeline-buffer-file-name-width-limit 80
    "The limit of the buffer-file-name width.

If `string-width' of `doom-modeline-buffer-file-name' is larger than the limit,
some information won't be displayed. It can be an integer or a float number. nil
means no limit."
    :type '(choice integer
            (const :tag "Disable" nil))
    :group 'doom-modeline)

  (require 'dash)

  (defun smart-abbreviate-filename (max-len filename)
    "Abbreviate FILENAME to MAX-LEN using a guided, index-based linear search.

This version is optimized for both its algorithm (heuristic) and
its implementation (index-passing) to minimize memory allocation
and CPU time."
    (if (<= (length filename) max-len)
        filename
      (let* ((basename (file-name-sans-extension filename))
             (extname (file-name-extension filename))
             (suffix (if extname (format ".%s" extname) ""))
             (base-len (length basename)))

        ;; Educated guess for the starting prefix length
        (let* ((initial-guess (- max-len (length suffix) 4))
               (start-len (max 0 (min base-len initial-guess))))

          (apply #'propertize
                 (abbreviate-from-len basename start-len max-len suffix)
                 (text-properties-at 0 filename))))))

  (defun abbreviate-from-len (basename current-len max-len suffix)
    "Tail-recursive helper using an integer length to avoid intermediate strings."
    (if (< current-len 0)
        ;; Safety net in case of an issue, should not be reached.
        (concat basename suffix)
      (let* ((marker (format "~%d" (- (length basename) current-len)))
             (candidate
              (concat
               (substring basename 0 current-len)
               marker
               suffix)))

        (if (<= (string-width candidate) max-len)
            ;; If it fits, this is our answer.
            candidate
          ;; If not, recurse with a shorter length. This is extremely cheap.
          (abbreviate-from-len basename (1- current-len) max-len suffix)))))

  (defadvice! custom-modeline (fn &rest args)
    :around #'doom-modeline-buffer-file-name
    (let ((choices
           (--map
            (let ((doom-modeline-buffer-file-name-style it))
              (funcall fn))
            '(relative-from-project
              truncate-with-project
              file-name-with-project
              relative-to-project
              file-name)))
          (limit doom-modeline-buffer-file-name-width-limit))
      (or (-some->> choices
            (--filter (< (string-width it) limit))
            (-max-by (-on #'> #'string-width)))
          (->> choices
               (-max-by (-on #'< #'string-width))
               (smart-abbreviate-filename limit))))))
