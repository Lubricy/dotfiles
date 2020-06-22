;;; +evil-commands.el --- description -*- lexical-binding: t; -*-

(after! persp-mode
  (define-key! persp-mode-map
    [remap evil-save-and-quit] #'+workspace/close-window-or-workspace
    [remap evil-quit] #'+workspace/close-window-or-workspace))

(provide '+evil-commands)
;;; +evil-commands.el ends here
