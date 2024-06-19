(after! chinese
  (defvar-local fcitx--input-method nil)
  (defun fcitx--activate-proc ()
    (unless fcitx--input-method
      (call-process fcitx-remote-command nil nil nil "-o")))
  (defun fcitx--deactivate-proc ()
    (setq fcitx--input-method
          (string-trim (shell-command-to-string (format "%s -n" fcitx-remote-command))))
    (call-process fcitx-remote-command nil nil nil "-c")))
