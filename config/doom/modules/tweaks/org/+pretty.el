(after! org-fancy-priorities
  (setq org-priority-faces '((65 . error) (66 . success) (67 . warning)))
  (setq org-fancy-priorities-list '("󰘃" "󰝔" "󰘄")))

(after! org
  (plist-put! +ligatures-extra-symbols
              :name    "❯"
              :header  "»"
              :begin  ""
              :end  ""
              :result ""
              :return "󰌑 "
              :yield "󱞹 "
              )
  (let* ((blocks
          '("comment"
            "example"
            "notes"))
         (ligatures
          '(:name   "#+name:"
            :header "#+header:"
            :header "#+headers:"
            :result "#+results:"))
         (+ligatures (append ligatures
                             (cl-loop for b in blocks
                                      append `(:begin ,(concat "#+begin_" b)
                                               :end ,(concat "#+end_" b))
                                      ))))
    (apply #'set-ligatures! 'org-mode
           (cl-loop for (k v) on +ligatures by #'cddr
                    append `(,k ,v ,k ,(upcase v))))))
