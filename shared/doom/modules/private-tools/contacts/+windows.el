(require 'json)

;; --- Configuration ---
(defvar ad-script-path (expand-file-name "~/.local/bin/query-ad.ps1")
  "Path to the PowerShell script that queries Active Directory.")

;; --- Core Function (Returns Data) ---
(defun ad-fetch-user-data (search-term)
  "Run the external PS1 script and return the results as a list of alists.
Returns nil if no results found or on error."
  (let ((buffer-name " *ad-json-output*") ;; Space at start = hidden buffer
        (args (list "-NoProfile"
                    "-NonInteractive"
                    "-ExecutionPolicy" "Bypass"
                    "-File" (expand-file-name ad-script-path)
                    search-term)))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (let ((coding-system-for-read 'utf-8)) ;; Match the PS1 encoding
        ;; Call PowerShell synchronously
        (apply 'call-process "powershell" nil t nil args)

        ;; Reset point to beginning of buffer to parse
        (goto-char (point-min))

        ;; Parse JSON.
        ;; :object-type 'alist  -> Converts JSON objects to ((key . val) ...)
        ;; :array-type 'list    -> Converts JSON arrays to (item1 item2 ...)
        (condition-case err
            (json-parse-buffer :object-type 'alist
                               :array-type 'list
                               :false-object nil)
          (json-parse-error
           (message "Error parsing AD JSON: %s" err)
           nil))))))

;;; Example
;;
;; (identity-provider-add! company
;;   :fetcher #'ad-fetch-user-data
;;   :alias corp
;;   :formats
;;   (
;;    samaccountname ?u
;;    uid            "%u"
;;    cn             ?c
;;    mail           ?m
;;    name           "%c"
;;    link           "[[del:%u][%c <%m>]]"
;;    --display      "%c\t%m"))
;;
;; (setq contact-default-identity-provider 'company)
