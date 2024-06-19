;;; +evil-commands.el --- description -*- lexical-binding: t; -*-

(after! persp-mode
  (define-key! persp-mode-map
    [remap evil-quit] #'kill-buffer-and-window))

(evil-ex-define-cmd "k[ill]" #'kill-this-buffer)
(provide '+evil-commands)
;;; +evil-commands.el ends here
