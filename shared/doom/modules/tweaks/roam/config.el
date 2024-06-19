(use-package! websocket
  :after org-roam)

(setq org-roam-capture-templates
      '(("d" "default" plain "%?" :target
         (file+head "default/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)))

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)
  ;; FIXME workaround since fd/rg follows .gitignore
  (setq org-roam-list-files-commands 'nil)
  (setq org-roam-dailies-capture-templates
        `(("d" "default" entry
           (file "templates/chat.org")
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n")))))

;; (use-package! delve
;;   :after org-roam
;;   :config
;;   (add-hook #'delve-mode-hook #'delve-compact-view-mode)
;;   (delve-global-minor-mode))
