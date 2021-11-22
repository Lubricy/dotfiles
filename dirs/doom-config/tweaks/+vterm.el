(after! vterm
  (defun lubricy/enter-vterm-copy-mode (&rest ARGS)
    (when (and (eq major-mode 'vterm-mode)
               (not (bound-and-true-p vterm-copy-mode)))
      (vterm-copy-mode 1)))
  (dolist (fn '(mouse-drag-region
                evil-mouse-drag-region
                evil-scroll-up
                evil-scroll-down
                evil-previous-line
                evil-next-line
                evil-backward-char
                evil-forward-char))
    (advice-add fn :before 'lubricy/enter-vterm-copy-mode))
  (add-hook! vterm-mode
    (when vterm-copy-mode
      (vterm-copy-mode -1))))
