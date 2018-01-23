;;; private/lubricy/+bindings.el -*- lexical-binding: t; -*-

(map! :en "C-<left>"    #'evil-window-left
      :en "C-<down>"    #'evil-window-down
      :en "C-<up>"      #'evil-window-up
      :en "C-<right>"   #'evil-window-right

      "C-x p"     #'doom/other-popup


      ;; --- <leader> -------------------------------------
      (:leader
        :desc "move left"     :en "<left>"   #'evil-window-left
        :desc "move right"    :en "<right>"  #'evil-window-right
        :desc "move up"       :en "<up>"     #'evil-window-up
        :desc "move down"     :en "<down>"   #'evil-window-down)
        (:desc "Workspace" :prefix "`"
          :desc "New workspace"            :n "c"   #'+workspace/new
          :desc "Kill all buffers"         :n "X"   #'doom/kill-all-buffers
          :desc "Delete session"           :n "d"   #'+workspace/kill-session
          :desc "Delete this workspace"    :n "x"   #'+workspace/delete)

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
        :n "v"         #'neotree-enter-vertical-split
        :n "s"         #'neotree-enter-horizontal-split
        :n "q"         #'neotree-hide
        :n "R"         #'neotree-refresh))
