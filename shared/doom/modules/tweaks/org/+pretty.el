;; (after! org-fancy-priorities
;;   (setq org-priority-faces '((65 . error) (66 . success) (67 . warning)))
;;   (setq org-fancy-priorities-list '("󰘃" "󰝔" "󰘄")))

;; (after! org
;;   (s)
;;   (plist-put! +ligatures-extra-symbols
;;               :name    "❯"
;;               :header  "»"
;;               :begin  ""
;;               :end  ""
;;               :result ""
;;               :return "󰌑 "
;;               :yield "󱞹 "
;;               )
;;   (let* ((blocks
;;           '("comment"
;;             "example"
;;             "notes"))
;;          (ligatures
;;           '(:name   "#+name:"
;;             :header "#+header:"
;;             :header "#+headers:"
;;             :result "#+results:"))
;;          (+ligatures (append ligatures
;;                              (cl-loop for b in blocks
;;                                       append `(:begin ,(concat "#+begin_" b)
;;                                                :end ,(concat "#+end_" b))
;;                                       ))))
;;     (apply #'set-ligatures! 'org-mode
;;            (cl-loop for (k v) on +ligatures by #'cddr
;;                     append `(,k ,v ,k ,(upcase v))))))

(after! org-modern
  (setq! org-modern-star 'replace
         org-modern-keyword '(("results" . "")
                              ("name" . "❯")
                              ("header" . "»")
                              (t . t))
         org-modern-block-name '(("src"  "»" "«")
                                 ("quote" "“" "”")
                                 ("example" "" "")
                                 (t . t))))
