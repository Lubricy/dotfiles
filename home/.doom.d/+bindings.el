;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

(map!
 :g "M-c" #'clipboard-kill-ring-save
 :g "M-v" #'clipboard-yank
 ;; <leader> -------------------------------------
 (:leader
   :desc "Left window"             :n "<left>"    #'evil-window-left
   :desc "Right window"            :n "<right>"   #'evil-window-right
   :desc "Up window"               :n "<up>"      #'evil-window-up
   :desc "Down window"             :n "<down>"    #'evil-window-down
   (:prefix "t"
    :desc "Adjust text size"      "t"   #'text-scale-adjust)
   (:prefix ("o" . "open")
     :desc "Google Search"      "g"   #'google-this)
   (:when (featurep! :ui workspaces)
     (:prefix ("TAB" . "workspace")
       :desc "New workspace"             "c"   #'+workspace/new
       :desc "Delete session"            "d"   #'+workspace/kill-session
       :desc "Delete this workspace"     "x"   #'+workspace/delete
       :desc "Rename workspace"          ","   #'+workspace/rename
       :desc "Restore last session"      "R"   #'+workspace/restore-last-session)))

 ;; <completion> ---------------------------------
 (:when (featurep! :completion ivy)
   (:after ivy
     :map ivy-minibuffer-map
     :g "M-v"   #'yank))

 ;; <drawer> -------------------------------------
 (:when (featurep! :ui treemacs)
   :after treemacs
   (:when (featurep! :editor evil)
    :map evil-treemacs-state-map
    :g [escape]  #'treemacs-quit
    :g "h"       #'treemacs-visit-node-horizontal-split
    :g "J"       #'treemacs-root-up
    :g "v"       #'treemacs-visit-node-vertical-split
    :g "c c"     #'treemacs-create-file))

 (:when (featurep! :lang org)
   :after org
   (:when (featurep! :editor evil)
    :after evil
    :map evil-org-mode-map
    :n "M-S-<left>"     #'org-do-promote
    :n "M-S-<right>"    #'org-do-demote
    :n "M-<left>"       #'org-promote-subtree
    :n "M-<right>"      #'org-demote-subtree
    :n "o"              (λ! (+org/insert-item 'below))
    :n "O"              (λ! (+org/insert-item 'above))
    :i [return]         (λ! (+org/insert-item 'below))
    :i [C-return]       #'org-return-indent)))

(evil-ex-define-cmd "k[ill]" #'kill-this-buffer)


;; <ugly hacks> ------------------------------------------
(after! treemacs
  (evil-define-key* 'treemacs treemacs-mode-map (kbd "h") nil))
