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
   :desc "tools" "t" #'gptel-tools
   :desc "add context" "c" #'gptel-context-add
   :desc "examine context" "C" #'gptel--suffix-context-buffer
   :desc "generate code" "q" #'relysium-edit-dwim
   :desc "keep all changes" "a" #'relysium-keep-all-changes
   :desc "discard all changes" "d" #'relysium-discard-all-changes
   :desc "clear buffer" "x" #'relysium-buffer-add-context
   :desc "add context" "b" #'relysium-buffer-add-context
   :desc "toggle window" "w" #'relysium-buffer-toggle-window)))

(map!
 (:leader
  (:prefix ("a" . "elysium"))))

;; HACK: RET conflict with org-mode
(after! gptel-transient
  (transient-suffix-put #'gptel-menu (kbd "RET") :key "<return>")
  (transient-suffix-put #'gptel-tools (kbd "RET") :key "<return>"))
