;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

(map!
 :g "M-c" #'clipboard-kill-ring-save
 :g "M-v" #'clipboard-yank
 ;; <leader> -------------------------------------
 (:leader
   :desc "Org capture"             "x"    #'org-capture
   :desc "Pop up scratch buffer"   "X"    #'doom/open-scratch-buffer
   :desc "Left window"             :n "<left>"    #'evil-window-left
   :desc "Right window"            :n "<right>"   #'evil-window-right
   :desc "Up window"               :n "<up>"      #'evil-window-up
   :desc "Down window"             :n "<down>"    #'evil-window-down
   (:prefix ("d" . "Get Things Done")
    :desc "add item to inbox"               "c" #'org-gtd-capture
    :desc "see what's on your plate today"  "a" #'org-agenda-list
    :desc "process entire inbox"            "p" #'org-gtd-process-inbox
    :desc "see all NEXT items"              "n" #'org-gtd-show-all-next
    :desc "show all stuck projects"         "s" #'org-gtd-show-stuck-projects
    :desc "finish editing"                  "f" #'org-gtd-clarify-finalize)
   (:prefix "t"
    :desc "Adjust text size"      "t"   #'text-scale-adjust
    :desc "Auto format on save"   "a"   #'format-all-mode)
   (:prefix ("l" . "link")
     :desc "Google Chrome"        "c"   #'org-mac-chrome-insert-frontmost-url
     :desc "Microsoft Outlook"    "o"   #'org-mac-outlook-message-insert-selected
     :desc "Finder"               "f"   #'org-mac-finder-insert-selected
     :desc "Jira"                 "j"   #'+ejira-insert-link)
   (:prefix ("n" . "notes")
     :desc "Org insert last link"      "p"   #'org-insert-last-stored-link)
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
     :g "M-v"    #'yank
     :g "C-u"    #'ivy-scroll-down-command
     :g "C-d"    #'ivy-scroll-up-command))

 ;; <drawer> -------------------------------------
 (:when (featurep! :ui treemacs)
   :after treemacs
   (:when (featurep! :editor evil)
    :map evil-treemacs-state-map
    :g [escape]  #'treemacs-quit
    :g "h"       #'treemacs-visit-node-horizontal-split
    :g "J"       #'treemacs-root-up
    :g "v"       #'treemacs-visit-node-vertical-split
    :g "c c"     #'treemacs-create-file
    :g "A"       #'treemacs-add-project-to-workspace
    :g "D"       #'treemacs-remove-project-from-workspace))

 (:when (featurep! :lang org)
   :after org
   (:when (featurep! :editor evil)
    :after evil
    :map evil-org-mode-map
    :n "M-S-<left>"     #'org-do-promote
    :n "M-S-<right>"    #'org-do-demote
    :n "M-<left>"       #'org-promote-subtree
    :n "M-<right>"      #'org-demote-subtree
    :n "j"              #'org-next-block
    :n "k"              #'org-previous-block)))
    ; :i [return]         #'org-return-indent
    ; :i [C-return]       #'org-return-indent)))

(evil-ex-define-cmd "k[ill]" #'kill-this-buffer)


;; <ugly hacks> ------------------------------------------
(after! treemacs
  (evil-define-key* 'treemacs treemacs-mode-map (kbd "h") nil))
