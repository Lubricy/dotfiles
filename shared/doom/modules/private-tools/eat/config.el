(use-package! eat
  :config
  ;; Set PowerShell as the default shell for eat on Windows.
  ;; Use "pwsh.exe" for PowerShell 7+ or "powershell.exe" for Windows PowerShell 5.1.
  (when (featurep :system 'windows)
    (setq eat-shell "pwsh.exe"))


  ;; ;; --- Keybindings ---
  ;; ;; Bind "C-c t" to open a new eat terminal in the current project's root directory.
  ;; ;; This is a convenient and easy-to-remember keybinding.
  ;; (map! :leader
  ;;       :desc "Open eat terminal" "t" #'eat)

  ;; You can also create a keybinding to open eat in a popup window,
  ;; similar to how Doom handles vterm.
  (defun +my/eat-popup ()
    "Open eat in a popup window."
    (interactive)
    (let ((display-buffer-alist
           '(("\\*eat\\*"
              (display-buffer-in-side-window)
              (side . bottom)
              (window-height . 0.3)))))
      (eat)))

                                        ; (map! :leader
                                        ;       :desc "Open eat popup" "o t" #'+my/eat-popup)

  ;; --- Behavior ---
  ;; Advise Eshell to use `eat` for visual commands like htop or vim.
  ;; This is one of eat's best features.
  ;; (add-hook! 'eshell-mode-hook #'eat-eshell-mode)
  (setq eshell-visual-commands
        '("htop" "vim" "nvim" "nano" "less" "git rebase -i" "python -i"))

  ;; When an eat buffer is killed, also kill the underlying shell process.
  (add-hook 'eat-final-hook #'kill-process))
