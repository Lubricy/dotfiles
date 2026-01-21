(cl-flet*
    ((concat-path (prefix suffix) (concat (file-name-as-directory prefix) suffix))
     (concat-paths (&rest args) (cl-reduce #'concat-path args)))
  (let*
      ((default-scoop-dir (concat-paths (getenv "USERPROFILE") "scoop"))
       (scoop-dir (or (getenv "SCOOP") default-scoop-dir))
       (appdata-dir (getenv "APPDATA"))
       (git-current (concat-paths scoop-dir "apps/git/current"))
       (git-bin (concat-paths git-current "bin"))
       (git-usr-bin (concat-paths git-current "usr/bin")))
    (add-to-list 'exec-path git-bin)
    (add-to-list 'exec-path git-usr-bin)
    (setq! shell-file-name (concat-paths git-bin "bash.exe"))
    (setq! explicit-shell-file-name (concat-paths git-bin "bash.exe"))
    (setq! python-shell-interpreter "python")
    (setq! treemacs-python-executable "python")
    (after! (lsp-mode)
      (require 'straight)
      (setq! lsp-roslyn--stdpipe-path
             (concat
              (straight--repos-dir (plist-get
                                    (gethash "lsp-mode" straight--recipe-cache)
                                    :local-repo))
              "clients/lsp-roslyn-stdpipe.ps1"))
      t)
    (after! envrc
      (envrc-global-mode -1))
    (after! jupyter
      (defadvice! +jupyter-locate-python-windows ()
        :override #'jupyter-locate-python
        (concat-paths appdata-dir "uv/tools/jupyter-core/Scripts/python.exe")))))
(after! tramp 
  (add-to-list 'tramp-methods
               `("wsl"
                 (tramp-login-program     "wsl")
                 (tramp-login-args        (("-d" "%h") ("--cd" "~") ("--") ("env" "/run/current-system/sw/bin/sh")))
                 (tramp-remote-shell      "/run/current-system/sw/bin/sh")
                 (tramp-remote-shell-args ("-i" "-c")))))
(after! consult
  (setq! consult-async-input-debounce 0.5)
  (setq! consult-async-input-throttle 0.5))
