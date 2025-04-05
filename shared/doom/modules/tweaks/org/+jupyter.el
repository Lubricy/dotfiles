(require 'cl-lib)

;; Customization group for the minor mode
(defgroup org-babel-abbreviate nil
  "Customization group for abbreviating Org Babel results."
  :group 'org-babel)

(defcustom org-babel-max-line-length 120
  "Maximum length of a line in Org code block output before wrapping."
  :type 'integer
  :group 'org-babel-abbreviate)

(defcustom org-babel-max-lines 40
  "Maximum number of lines in Org code block output before abbreviating."
  :type 'integer
  :group 'org-babel-abbreviate)

(defcustom org-babel-max-list-length 20
  "Maximum number of elements in list results before abbreviation."
  :type 'integer
  :group 'org-babel-abbreviate)

;; Helper function to wrap a single line
(defun my-wrap-line (line max-length)
  "Wrap LINE into multiple lines, each no longer than MAX-LENGTH."
  (let ((lines '())
        (start 0))
    (while (> (length line) start)
      (let ((end (min (length line) (+ start max-length))))
        (push (substring line start end) lines)
        (setq start end)))
    (nreverse lines)))

;; Function to abbreviate a string
(defun my-abbreviate-string (str max-lines max-line-length)
  "Abbreviate STR by wrapping long lines and limiting the number of lines."
  (let ((lines (split-string str "\n"))
        (wrapped-lines '()))
    (dolist (line lines)
      (if (> (length line) max-line-length)
          (setq wrapped-lines (append wrapped-lines (my-wrap-line line max-line-length)))
        (setq wrapped-lines (append wrapped-lines (list line)))))
    (if (> (length wrapped-lines) max-lines)
        (let* ((half (- max-lines 3))
               (first-part (cl-subseq wrapped-lines 0 half))
               (last-part (cl-subseq wrapped-lines (- (length wrapped-lines) 3))))
          (setq wrapped-lines (append
                               first-part
                               '(" ...")
                               '(" +----------------------------------------------------------------+")
                               '("/ WARNING: Long content abbreviated by `org-babel-abbreviate-mode`. \\")
                               '("\\          Adjust `org-babel-max-lines` to see more...              /")
                               '(" +----------------------------------------------------------------+")
                               '(" ...")
                               last-part
                               ))))
    (mapconcat 'identity wrapped-lines "\n")))

(defun modify-nested-plist (plist fn)
  "Recursively traverse a nested PLIST, applying FN to values associated with the key :value.
PLIST is a property list that may contain nested plists. FN is a function that takes a single
argument (the value associated with :value) and returns a modified value. Returns a new plist
with the modifications applied."
  (let ((result '())
        (current plist))
    (while current
      (let ((key (car current))
            (value (cadr current)))
        (if (plistp value)
            (setq result (append (list key (modify-nested-plist value fn)) result))
          (if (eq key :value)
              (setq result (append (list key (funcall fn value)) result))
            (setq result (append (list key value) result)))))
      ;; Move to the next key-value pair
      (setq current (cddr current)))
    result))

(defun my-filter-org-babel-insert-result (args)
  "Filter function to abbreviate long results before insertion."
  (cl-destructuring-bind (req context result) args
    (list req context
          (if (stringp result)
              (my-abbreviate-string result org-babel-max-lines org-babel-max-line-length)
            (modify-nested-plist
             result
             (lambda (value)
               (my-abbreviate-string value org-babel-max-lines org-babel-max-line-length)))))))

;; Minor mode definition
(define-minor-mode org-babel-abbreviate-mode
  "Toggle abbreviation of long Org code block results.
When enabled, long lines are wrapped, and outputs with many lines or list elements are abbreviated."
  :global t
  :lighter " OrgAbbr"
  (if org-babel-abbreviate-mode
      (advice-add 'jupyter-org--insert-result :filter-args #'my-filter-org-babel-insert-result)
    (advice-remove 'jupyter-org--insert-result #'my-filter-org-babel-insert-result)))

(after! ob-jupyter
  (org-babel-abbreviate-mode t)
  (setq org-babel-default-header-args:jupyter-python
        '((:pandoc . t))))
