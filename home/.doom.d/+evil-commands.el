;;; +evil-commands.el --- description -*- lexical-binding: t; -*-

(after! persp-mode
  (define-key! persp-mode-map
    [remap evil-quit] #'kill-current-buffer))

(provide '+evil-commands)
;;; +evil-commands.el ends here
