;;; tools/gtd/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(use-package! org-gtd
  :after-call org-capture
  :config
  ;; where org-gtd will put its files. This value is also the default one.
  (setq org-gtd-directory "~/gtd/")
  (add-to-list 'org-agenda-files  "~/gtd/")
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "STRT(s)" "WAIT(w)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")
                            (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")))
  ;; package: https://github.com/Malabarba/org-agenda-property
  ;; this is so you can see who an item was delegated to in the agenda
  (setq org-agenda-property-list '("DELEGATED_TO"))
  ;; I think this makes the agenda easier to read
  (setq org-agenda-property-position 'next-line)
  ;; package: https://www.nongnu.org/org-edna-el/
  ;; org-edna is used to make sure that when a project task gets DONE,
  ;; the next TODO is automatically changed to NEXT.
  (setq org-edna-use-inheritance t)
  (org-edna-load)
  :bind
  (("C-c d c" . org-gtd-capture) ;; add item to inbox
   ("C-c d a" . org-agenda-list) ;; see what's on your plate today
   ("C-c d p" . org-gtd-process-inbox) ;; process entire inbox
   ("C-c d n" . org-gtd-show-all-next) ;; see all NEXT items
   ;; see projects that don't have a NEXT item
   ("C-c d s" . org-gtd-show-stuck-projects)
   ;; the keybinding to hit when you're done editing an item in the
   ;; processing phase
   ("C-c d f" . org-gtd-clarify-finalize)))

(after! org-gtd
  (add-to-list 'org-capture-templates
               '("i" "GTD item"
                 entry
                 (file (lambda () (org-gtd--path org-gtd-inbox-file-basename)))
                 "* %?\n%U\n\n  %i"
                 :kill-buffer t))
  (add-to-list 'org-capture-templates
               '("e" "GTD item with link to where you are in emacs now"
                 entry
                 (file (lambda () (org-gtd--path org-gtd-inbox-file-basename)))
                 "* %?\n%U\n\n  %i\n  %a"
                 :kill-buffer t))
  (add-to-list 'org-capture-templates
               '("l" "GTD item with link to current Chrome web page"
                 entry
                 (file (lambda () (org-gtd--path org-gtd-inbox-file-basename)))
                 "* %?\n%U\n\n  %i\n  %(org-mac-chrome-get-frontmost-url)"
                 :kill-buffer t))
  (add-to-list 'org-capture-templates
               '("m" "GTD item with link to current Outlook mail message"
                 entry
                 (file (lambda () (org-gtd--path org-gtd-inbox-file-basename)))
                 "* %?\n%U\n\n  %i\n  %(org-mac-outlook-message-get-links)"
                 :kill-buffer t)))
