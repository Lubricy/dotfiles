;;; private/lubricy/+bindings.el -*- lexical-binding: t; -*-


(map!
      ;; --- <leader> -------------------------------------
      (:leader
        :desc "new terminal at right"   :nv "\\" (λ! (evil-window-vsplit)(+term/open))
        :desc "new terminal at bottom"  :nv "-" (λ! (evil-window-split)(+term/open))

        :desc "Left window"             :n "<left>"    #'evil-window-left
        :desc "Right window"            :n "<right>"   #'evil-window-right
        :desc "Up window"               :n "<up>"      #'evil-window-up
        :desc "Down window"             :n "<down>"    #'evil-window-down
        (:desc "workspace" :prefix "TAB"
          :desc "Display tab bar"          :n "TAB" #'+workspace/display
          :desc "New workspace"            :n "n"   #'+workspace/new
          :desc "New workspace"            :n "c"   #'+workspace/new
          :desc "Load workspace from file" :n "l"   #'+workspace/load
          :desc "Load last session"        :n "L"   (λ! (+workspace/load-session))
          :desc "Save workspace to file"   :n "s"   #'+workspace/save
          :desc "Autosave current session" :n "S"   #'+workspace/save-session
          :desc "Switch workspace"         :n "."   #'+workspace/switch-to
          :desc "Switch workspace"         :n ","   #'+workspace/rename
          :desc "Kill all buffers"         :n "X"   #'doom/kill-all-buffers
          :desc "Delete session"           :n "d"   #'+workspace/kill-session
          :desc "Delete this workspace"    :n "x"   #'+workspace/delete
          :desc "Load session"             :n "L"   #'+workspace/load-session
          :desc "Next workspace"           :n "]"   #'+workspace/switch-right
          :desc "Previous workspace"       :n "["   #'+workspace/switch-left
          :desc "Switch to 1st workspace"  :n "1"   (λ! (+workspace/switch-to 0))
          :desc "Switch to 2nd workspace"  :n "2"   (λ! (+workspace/switch-to 1))
          :desc "Switch to 3rd workspace"  :n "3"   (λ! (+workspace/switch-to 2))
          :desc "Switch to 4th workspace"  :n "4"   (λ! (+workspace/switch-to 3))
          :desc "Switch to 5th workspace"  :n "5"   (λ! (+workspace/switch-to 4))
          :desc "Switch to 6th workspace"  :n "6"   (λ! (+workspace/switch-to 5))
          :desc "Switch to 7th workspace"  :n "7"   (λ! (+workspace/switch-to 6))
          :desc "Switch to 8th workspace"  :n "8"   (λ! (+workspace/switch-to 7))
          :desc "Switch to 9th workspace"  :n "9"   (λ! (+workspace/switch-to 8))
          :desc "Switch to last workspace" :n "0"   #'+workspace/switch-to-last)        
        (:desc "search" :prefix "/"
          :desc "Swiper"                :nv "/" #'swiper
          :desc "Ag"                    :nv "f" #'+ivy:ag
          :desc "Imenu"                 :nv "i" #'imenu
          :desc "Imenu across buffers"  :nv "I" #'imenu-anywhere
          :desc "Online providers"      :nv "o" #'+lookup/online-select))

      ;; --- Plugin bindings ------------------------------
      ;; auto-yasnippet
      :i  [C-tab] #'aya-expand
      :nv [C-tab] #'aya-create

      ;; company-mode (vim-like omnicompletion)
      :i "C-SPC"  #'+company/complete
        ;; Automatically applies to `company-filter-map'

      ;; neotree
      (:after neotree
        :map neotree-mode-map
        :n "g"         nil
        :n [tab]       #'neotree-stretch-toggle
        :n "i"         #'neotree-hidden-file-toggle
        :n "RET"       #'neotree-enter
        :n [backspace] #'evil-window-prev
        :n "c"         #'neotree-create-node
        :n "r"         #'neotree-rename-node
        :n "d"         #'neotree-delete-node
        :n "j"         #'neotree-next-line
        :n "k"         #'neotree-previous-line
        :n "h"         #'+neotree/collapse-or-up
        :n "l"         #'+neotree/expand-or-open
        :n "J"         #'neotree-select-next-sibling-node
        :n "K"         #'neotree-select-previous-sibling-node
        :n "H"         #'neotree-select-up-node
        :n "L"         #'neotree-select-down-node
        :n "G"         #'evil-goto-line
        :n "gg"        #'evil-goto-first-line
        :n "yy"        #'neotree-copy-filepath-to-yank-ring
        :n "v"         #'neotree-enter-vertical-split
        :n "s"         #'neotree-enter-horizontal-split
        :n "q"         #'neotree-hide
        (:prefix "m"
          :desc "New ..."     :n "a"       #'neotree-create-node
          :desc "Copy..."     :n "c"       #'neotree-copy-node
          :desc "Move..."     :n "m"       #'neotree-rename-node
          :desc "Delete"      :n "d"       #'neotree-delete-node)
        :n "R"         #'neotree-refresh))


;; --- <ex-commands> ------------------------------
(ex! "q" #'+workspace/close-window-or-workspace)
