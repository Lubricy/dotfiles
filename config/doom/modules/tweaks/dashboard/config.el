(defun lubricy/quickload-session ()
  (interactive)
  (doom/quickload-session t))

(setq +doom-dashboard-menu-sections
      '(("Recently opened files"
         :icon (nerd-icons-faicon "nf-fa-file_text" :face 'doom-dashboard-menu-title)
         :action recentf-open-files)
        ("Reload last session"
         :icon (nerd-icons-octicon "nf-oct-history" :face 'doom-dashboard-menu-title)
         :when (cond ((modulep! :ui workspaces)
                      (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
                     ((require 'desktop nil t)
                      (file-exists-p (desktop-full-file-name))))
         :action lubricy/quickload-session)
        ("Open org-gtd-engage"
         :icon (nerd-icons-octicon "nf-oct-calendar" :face 'doom-dashboard-menu-title)
         :when (fboundp 'org-gtd-engage)
         :action org-gtd-engage)
        ("Open project"
         :icon (nerd-icons-octicon "nf-oct-briefcase" :face 'doom-dashboard-menu-title)
         :action projectile-switch-project)
        ("Jump to bookmark"
         :icon (nerd-icons-octicon "nf-oct-bookmark" :face 'doom-dashboard-menu-title)
         :action bookmark-jump)
        ("Open private configuration"
         :icon (nerd-icons-octicon "nf-oct-tools" :face 'doom-dashboard-menu-title)
         :when (file-directory-p doom-user-dir)
         :action doom/open-private-config)
        ("Open documentation"
         :icon (nerd-icons-octicon "nf-oct-book" :face 'doom-dashboard-menu-title)
         :action doom/help)))

(setq fancy-splash-image (concat doom-user-dir "splash/doom-emacs-flugo-slant_out_purple-small.png"))
