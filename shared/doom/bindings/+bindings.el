;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

(when (modulep! :editor format +onsave)
  (setq +enable-global-format-all t))

(map! :map transient-map
      :ng "ESC" #'transient-quit-one
      :ng "q" #'execute-extended-command)


(map!
 :i "S-SPC" #'completion-at-point
 :g "M-c" #'clipboard-kill-ring-save
 :g "M-v" #'clipboard-yank
 ;; <leader> -------------------------------------
 (:leader
  :desc "Org capture"             "x"    (cmd! (org-capture 'nil "i"))
  :desc "Pop up scratch buffer"   "X"    #'doom/open-scratch-buffer
  :desc "Left window"             :n "<left>"    #'evil-window-left
  :desc "Right window"            :n "<right>"   #'evil-window-right
  :desc "Up window"               :n "<up>"      #'evil-window-up
  :desc "Down window"             :n "<down>"    #'evil-window-down
  (:prefix ("c" . "code")
   :desc "toggle between implementation and test"   "t" #'projectile-toggle-between-implementation-and-test)
  (:prefix ("b" . "buffer")
   :desc "view raw buffer"                 "R" (cmd! (fundamental-mode)
                                                     (revert-buffer nil nil t))
   :desc "Auto format current buffer"      "f" #'apheleia-format-buffer)
  (:when (modulep! :completion ivy)
    (:prefix ("b" . "buffer")
     :desc "Force show buffer"              "F" #'+ivy/switch-buffer-other-window
     :desc "Force show workspace buffer"    "w" #'+ivy/switch-workspace-buffer-other-window
     :desc "Switch to workspace buffer"     "W" #'+ivy/switch-workspace-buffer))
  (:when (modulep! :private-tools gtd)
    (:prefix ("d" . "Get Things Done")
     :desc "capture item"                    "c" #'+org-gtd-dwim
     :desc "capture item"                    "C" #'org-capture
     :desc "see what's on your plate today"  "a" #'org-agenda-list
     :desc "process entire inbox"            "p" #'org-gtd-process-inbox
     :desc "see all NEXT items"              "n" #'org-gtd-show-all-next
     :desc "see all TODO items"              "t" #'org-todo-list
     :desc "show all stuck projects"         "s" #'org-gtd-review-stuck-projects
     :desc "show all stuck projects"         "h" #'org-gtd-review-stuck-habit-items
     :desc "gtd engage"                      "e" #'org-gtd-engage
     :desc "open inbox"                      "I" #'lubricy/goto-gtd-inbox-file
     :desc "punch in"                        "i" #'lubricy/punch-in
     :desc "punch out"                       "o" #'lubricy/punch-out
     :desc "finish editing"                  "d" #'org-gtd-organize))
  (:prefix ("t" . "toggle")
   :desc "Adjust text size"      "t"   #'text-scale-adjust
   :desc "Auto format on save"   "a"   #'apheleia-mode
   :desc "Auto format on save"   "A"   #'apheleia-global-mode

   :desc "line number"           "n"   #'display-line-numbers-mode
   :desc "tree silde"            "P"   #'org-tree-slide-mode
   :desc "prettify symbols"      "p"   #'global-prettify-symbols-mode
   (:when (modulep! :private-tools dirvish)
     :desc "Tree Follow" "o" #'dirvish-side-follow-mode)
   (:when (modulep! :private-tools git-utils)
     :desc "git blame" "B" #'global-blamer-mode))
  (:prefix ("l" . "link")
   :desc "People"               "p"   #'contact/insert
   (:when IS-MAC
     :desc "Google Chrome"        "c"   #'org-mac-link-chrome-insert-frontmost-url
     :desc "Microsoft Outlook"    "o"   #'org-mac-link-outlook-message-insert-selected
     :desc "Finder"               "f"   #'org-mac-link-finder-insert-selected))
  (:prefix ("o" . "open")
   :desc "Google Search"      "g"   #'google-this
   (:when (modulep! :private-tools dirvish)
     :desc "Project sidebar"              "p" #'dirvish-side
     :desc "Find file in project sidebar" "P" (cmd!
                                               (if  (dirvish-side--session-visible-p)
                                                   (dirvish-side--auto-jump)
                                                 (dirvish-side)))))
  (:prefix ("q" . "quit/session")
   :desc "Restore last session"      "l" #'lubricy/quickload-session)
  (:when (modulep! :private-tools browser)
    (:prefix ("s" . "search")
     :desc "Browser History"      "h"   #'browser-hist-search))
  (:when (modulep! :ui workspaces)
    (:prefix ("TAB" . "workspace")
     :desc "New workspace"             "c"   #'+workspace/new
     :desc "Delete session"            "D"   #'+workspace/kill-session
     :desc "Delete this workspace"     "x"   #'+workspace/kill
     :desc "Rename workspace"          ","   #'+workspace/rename
     :desc "swap left"                 "{"   #'+workspace/swap-left
     :desc "swap right"                "}"   #'+workspace/swap-right
     :desc "Restore last session"      "R"   #'+workspace/restore-last-session))
  (:prefix ("n" . "notes")
   :desc "Org insert last link"      "p"   #'org-insert-last-stored-link
   (:when (modulep! :lang org +noter)
     :desc "org noter" "n" #'org-noter)
   (:when (modulep! :tweaks roam)
     (:prefix ("r" . "roam")
      :desc "Create headline" "h" #'+org-roam-refile-or-create
      :desc "Open Roam UI"    "v" (cmd! (org-roam-ui-mode t))
      :desc "Stop Roam UI"    "V" (cmd! (org-roam-ui-mode 'toggle))
      )))

  (:prefix ("j" . "journal")
   :desc "Today"                    "j" #'org-roam-dailies-capture-today
   :desc "Tomorrow"                 "n" #'org-roam-dailies-capture-tomorrow
   :desc "Yesterday"                "p" #'org-roam-dailies-capture-yesterday
   :desc "Date"                    "d" #'org-roam-dailies-capture-date
   (:prefix ("g" . "goto")
    :desc "Today"                    "g" #'org-roam-dailies-goto-today
    :desc "Tomorrow"                 "n" #'org-roam-dailies-goto-tomorrow
    :desc "Yesterday"                "p" #'org-roam-dailies-goto-yesterday
    :desc "Forward"                  "f" #'org-roam-dailies-goto-next-note
    :desc "Backward"                 "b" #'org-roam-dailies-goto-previous-note
    :desc "Date"                     "d" #'org-roam-dailies-goto-date
    )))
 ;; <localleader> -------------------------------------
 (:localleader
  (:prefix ("d" . "debug")
   :desc "debug"      "d" #'dap-debug
   :desc "breakpoint" "b" #'dap-breakpoint-toggle))

 ;; <completion> ---------------------------------
 (:when (modulep! :completion ivy)
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
 (:when (modulep! :ui treemacs)
   :after treemacs
   (:when (modulep! :editor evil)
     :map evil-treemacs-state-map
     :g [escape]  #'treemacs-quit
     :g "h"       #'treemacs-visit-node-horizontal-split
     :g "J"       #'treemacs-root-up
     :g "v"       #'treemacs-visit-node-vertical-split
     :g "c c"     #'treemacs-create-file
     :g "A"       #'treemacs-add-project-to-workspace
     :g "D"       #'treemacs-remove-project-from-workspace))

 (:when (modulep! :lang org)
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
   (:when (modulep! :editor evil)
     :after evil
     :map evil-org-mode-map
     :n "M-S-<left>"     #'org-do-promote
     :n "M-S-<right>"    #'org-do-demote
     :n "M-<left>"       #'org-promote-subtree
     :n "M-<right>"      #'org-demote-subtree
     :n "j"              #'org-next-block
     :n "k"              #'org-previous-block)))
