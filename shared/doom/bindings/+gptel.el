(map!
 (:leader
  (:prefix ("t" . "toggle")
   :desc "auto code generation" "e" #'starhugger-auto-mode)))

(map!
 :map starhugger-inlining-mode-map
 :gm "SPC" (starhugger-inline-menu-item #'starhugger-accept-suggestion-by-line)
 :g "<RET>" (starhugger-inline-menu-item #'starhugger-accept-suggestion)
 :g "n" (starhugger-inline-menu-item #'starhugger-show-next-suggestion)
 :g "p" (starhugger-inline-menu-item #'starhugger-show-prev-suggestion)
 :g "[" (starhugger-inline-menu-item #'starhugger-show-next-suggestion)
 :g "]" (starhugger-inline-menu-item #'starhugger-show-prev-suggestion))

(map!
 :g "C-c C-g" #'gptel-send
 :g "C-c g" #'gptel-send
 (:leader
  (:prefix ("i" . "insert")
   :desc "generative AI" "g" #'gptel-send)
  (:prefix ("e" . "generate")
   :desc "send prompt" "e" #'gptel-send
   :desc "generative complete" "r" #'starhugger-trigger-suggestion
   :desc "menu" "m" #'gptel-menu
   :desc "add context" "c" #'gptel-context-add
   :desc "examine context" "C" #'gptel--suffix-context-buffer)))
