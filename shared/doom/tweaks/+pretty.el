(plist-put! +ligatures-extra-symbols
            :return "󰌑 "
            :yield "󱞹 "

            :union "⋃"
            :intersect "⋂"
            :top "⊤"
            :bottom "⊥"
            :diff "∖"
            :tuple "⨂"
            :pipe "ǀ" ;; FIXME: find a non-private char
            :dot "•"
            )


;; ;; HACK: taken from github:doomemacs/doomemacs::modules/lang/javascript/config.el
;; (dolist (feature '(rjsx-mode
;;                    typescript-mode
;;                    web-mode
;;                    (nodejs-repl-mode . nodejs-repl)))
;;   (let ((pkg  (or (cdr-safe feature) feature))
;;         (mode (or (car-safe feature) feature)))
;;     (with-eval-after-load pkg
;;       (set-ligatures! mode 'nil) ;; HACK: clear everything in `mode`
;;       (set-ligatures! mode
;;         ;; Functional
;;         :def "function"
;;         :lambda "() =>"
;;         :composition "compose"
;;         ;; Types
;;         :null "null"
;;         :null "void"
;;         :true "true" :false "false"
;;         :str "string" :float "number"
;;         :bottom "undefined"
;;         ;; Flow
;;         :not "!"
;;         :and "&&" :or "||"
;;         :for "for"
;;         :return "return"
;;         ;; Other
;;         :yield "yield"))))
