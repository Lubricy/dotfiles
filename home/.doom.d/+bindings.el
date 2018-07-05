;;; private/lubricy/+bindings.el -*- lexical-binding: t; -*-


;;; Code:
(map!
 ;; unset default bindings

 ;; --- <leader> -------------------------------------
 (:leader
   :desc "new terminal at right"   :nv "\\" (λ! (evil-window-vsplit)(+term/open))
   :desc "new terminal at bottom"  :nv "-" 'eshell

   :desc "Left window"             :n "<left>"    #'evil-window-left
   :desc "Right window"            :n "<right>"   #'evil-window-right
   :desc "Up window"               :n "<up>"      #'evil-window-up
   :desc "Down window"             :n "<down>"    #'evil-window-down
   :desc "Down window"             :n "<down>"    #'evil-window-down

   (:desc "open" :prefix "o"
     :desc "jupyter notbook"         :n "J"   #'ein:notebooklist-login
     :desc "jupyter notbook"         :n "j"   #'ein:notebooklist-open)
   (:desc "quit" :prefix "q"
     :desc "Kill all buffers"         :n "x"   #'doom/kill-all-buffers)
   (:desc "workspace" :prefix [tab]
     :desc "New workspace"            :n "c"   #'+workspace/new
     :desc "Load workspace from file" :n "l"   #'+workspace/load
     :desc "Load last session"        :n "L"   (λ! (+workspace/load-session))
     :desc "Save workspace to file"   :n "s"   #'+workspace/save
     :desc "Autosave current session" :n "S"   #'+workspace/save-session
     :desc "Switch workspace"         :n "."   #'+workspace/switch-to
     :desc "Delete session"           :n "X"   #'+workspace/kill-session
     :desc "Delete this workspace"    :n "x"   #'+workspace/delete
     :desc "Load session"             :n "L"   #'+workspace/load-session
     :desc "Rename workspace"         :n ","   #'+workspace/rename)

   (:desc "search" :prefix "/"
     :desc "Swiper"                :nv "/" #'swiper
     :desc "Ag"                    :nv "f" #'+ivy:ag
     :desc "Imenu"                 :nv "i" #'imenu
     :desc "Imenu across buffers"  :nv "I" #'imenu-anywhere
     :desc "Online providers"      :nv "o" #'+lookup/online-select))
 (:prefix "`"
   :n "`"   #'+workspace/display
   :n "c"   #'+workspace/new
   :n "l"   #'+workspace/load
   :n "L"   (λ! (+workspace/load-session))
   :n "s"   #'+workspace/save
   :n "S"   #'+workspace/save-session
   :n "."   #'+workspace/switch-to
   :n "X"   #'+workspace/kill-session
   :n "x"   #'+workspace/delete
   :n ","   #'+workspace/rename
   :n "."   #'+workspace/switch-to)

 ;; auto-yasnippet
 :i  [C-tab] #'aya-expand
 :nv [C-tab] #'aya-create)


 ;; --- Plugin bindings ------------------------------
 ;; neotree

(defun +ein:delete-at-point ()
  "Delete cell or content."
  (interactive)
  ((if (ein:worksheet-set-kernel))))

(map!
  (:after neotree
   :map neotree-mode-map
   :n "g"         nil
   :n [tab]       #'neotree-stretch-toggle
   :n "RET"       #'neotree-enter
   :n [backspace] #'evil-window-prev
   :n "c"         nil
   :n "cd"        #'neotree-change-root
   :n "d"         nil
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
   :n "y"        #'neotree-copy-filepath-to-yank-ring
   :n "v"         #'neotree-enter-vertical-split
   :n "s"         #'neotree-enter-horizontal-split
   :n "q"         #'neotree-hide
   (:prefix "m"
     :desc "New ..."     :n "a"       #'neotree-create-node
     :desc "Copy..."     :n "c"       #'neotree-copy-node
     :desc "Move..."     :n "m"       #'neotree-rename-node
     :desc "Delete"      :n "d"       #'neotree-delete-node)
   :n "r"         #'neotree-refresh))
(map!
  (:after ein
    :map* ein:notebook-mode-map
    :nm "RET"    #'ein:worksheet-execute-cell-and-goto-next
    :nm "d"      #'ein:worksheet-kill-cell
    :nm "t"      #'ein:worksheet-change-cell-type
    :nm "y"      #'ein:worksheet-copy-cell
    :nm "p"      #'ein:worksheet-yank-cell
    :nm "<up>"   #'ein:worksheet-goto-prev-input
    :nm "<down>" #'ein:worksheet-goto-next-input
    :nm [tab]    #'ein:worksheet-toggle-output
    :nm "O"    #'ein:worksheet-insert-cell-above
    :nm "o"    #'ein:worksheet-insert-cell-below
    :nm "K"    #'ein:worksheet-set-kernel
    :nm "U"    #'ein:worksheet-insert-last-input-history
    :nm "U"    #'ein:worksheet-insert-last-input-history
    :nm "C-J"    #'ein:worksheet-merge-cell
    ))

;; --- <ex-commands> ------------------------------

(provide '+bindings)
;;; +bindings.el ends here
