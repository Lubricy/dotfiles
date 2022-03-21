;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

(when (featurep! :editor format +onsave)
  (setq +enable-global-format-all t))

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
  (:prefix ("c" . "code")
   :desc "toggle between implementation and test"   "t" #'projectile-toggle-between-implementation-and-test)
  (:prefix ("b" . "buffer")
   :desc "show buffer"                     "b" #'display-buffer
   :desc "view raw buffer"                 "R" (cmd! (fundamental-mode)
                                                     (revert-buffer nil nil t))
   :desc "Auto format current buffer"      "f" #'format-all-buffer)
  (:when (featurep! :completion ivy)
   (:prefix ("b" . "buffer")
    :desc "Force show buffer"              "F" #'+ivy/switch-buffer-other-window
    :desc "Force show workspace buffer"    "w" #'+ivy/switch-workspace-buffer-other-window
    :desc "Switch to workspace buffer"     "W" #'+ivy/switch-workspace-buffer))
  (:when (featurep! :private-tools gtd)
   (:prefix ("d" . "Get Things Done")
    :desc "add item to inbox"               "c" #'org-gtd-capture
    :desc "see what's on your plate today"  "a" #'org-agenda-list
    :desc "process entire inbox"            "p" #'org-gtd-process-inbox
    :desc "see all NEXT items"              "n" #'org-gtd-show-all-next
    :desc "show all stuck projects"         "s" #'org-gtd-show-stuck-projects
    :desc "gtd engage"                      "e" #'org-gtd-engage
    :desc "punch in"                        "i" #'lubricy/punch-in
    :desc "punch out"                       "o" #'lubricy/punch-out
    :desc "finish editing"                  "d" #'org-gtd-choose))
  (:prefix ("t" . "toggle")
   :desc "Adjust text size"      "t"   #'text-scale-adjust
   :desc "Auto format on save"   "a"   #'format-all-mode
   :desc "Auto format on save"   "A"   (cmd!
                                        (if (bound-and-true-p +enable-global-format-all)
                                            (progn
                                              (remove-hook! after-change-major-mode #'+format-enable-on-save-maybe-h)
                                              (setq +enable-global-format-all 'nil)
                                              (message "Format-All mode disabled globally"))
                                          (add-hook! after-change-major-mode #'+format-enable-on-save-maybe-h)
                                          (setq +enable-global-format-all t)
                                          (message "Format-All mode enabled globally")))

   :desc "line number"           "n"   #'display-line-numbers-mode
   :desc "tree silde"            "P"   #'org-tree-slide-mode
   :desc "prettify symbols"      "p"   #'global-prettify-symbols-mode
   (:when (featurep! :private-tools blamer)
    :desc "git blame" "B" #'global-blamer-mode))
  (:when IS-MAC
   (:prefix ("l" . "link")
    :desc "Google Chrome"        "c"   #'org-mac-chrome-insert-frontmost-url
    :desc "Microsoft Outlook"    "o"   #'org-mac-outlook-message-insert-selected
    :desc "Finder"               "f"   #'org-mac-finder-insert-selected))
  (:prefix ("o" . "open")
   :desc "Google Search"      "g"   #'google-this)
  (:when (featurep! :ui workspaces)
   (:prefix ("TAB" . "workspace")
    :desc "New workspace"             "c"   #'+workspace/new
    :desc "Delete session"            "D"   #'+workspace/kill-session
    :desc "Delete this workspace"     "x"   #'+workspace/delete
    :desc "Rename workspace"          ","   #'+workspace/rename
    :desc "swap left"                 "{"   #'+workspace/swap-left
    :desc "swap right"                "}"   #'+workspace/swap-right
    :desc "Restore last session"      "R"   #'+workspace/restore-last-session))
  (:prefix ("n" . "notes")
   :desc "Org insert last link"      "p"   #'org-insert-last-stored-link
   (:when (featurep! :lang org +noter)
    :desc "org noter" "n" #'org-noter)
   (:when (featurep! :tweaks roam)
    (:prefix ("r" . "roam")
     :desc "Create headline" "h" #'+org-roam-refile-or-create
     :desc "Open Roam UI"    "v" (cmd! (org-roam-ui-mode t))
     :desc "Stop Roam UI"    "V" (cmd! (org-roam-ui-mode 'toggle))
     ))))

 ;; <completion> ---------------------------------
 (:when (featurep! :completion ivy)
  (:after ivy
   :map ivy-minibuffer-map
   :g "M-v"    #'yank
   :g "C-u"    #'ivy-scroll-down-command
   :g "C-d"    #'ivy-scroll-up-command
   :g "S-<return>"    #'ivy-immediate-done
   :g "C-<return>"    #'ivy-dispatching-done
   :g "C-M-<return>"    #'ivy-dispatching-call
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
  (:localleader
   :map org-mode-map
   :g "Q" (cmd!
           (let
               ((org-complete-tags-always-offer-all-agenda-tags t)
                (org-agenda-files (directory-files-recursively
                                   org-directory
                                   "org$")))
             (counsel-org-tag))))
  (:when (featurep! :editor evil)
   :after evil
   :map evil-org-mode-map
   :n "M-S-<left>"     #'org-do-promote
   :n "M-S-<right>"    #'org-do-demote
   :n "M-<left>"       #'org-promote-subtree
   :n "M-<right>"      #'org-demote-subtree
   :n "j"              #'org-next-block
   :n "k"              #'org-previous-block)))

