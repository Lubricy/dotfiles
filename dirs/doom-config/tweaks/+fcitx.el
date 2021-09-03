(after! chinese
  (defvar-local fcitx--input-method nil)
  (defun fcitx--activate-proc ()
    (message "get fcitx--input-method")
    (message (pp fcitx--input-method))
    (if fcitx--input-method
        (progn
          (message "true")
          (message (pp (call-process fcitx-remote-command nil nil nil "-s" fcitx--input-method))))
      (call-process fcitx-remote-command nil nil nil "-o")))
  (defun fcitx--deactivate-proc ()
    (setq fcitx--input-method (string-trim (shell-command-to-string (format "%s -n" fcitx-remote-command))))
    (message "set fcitx--input-method")
    (message (pp fcitx--input-method))
    (call-process fcitx-remote-command nil nil nil "-c")))
