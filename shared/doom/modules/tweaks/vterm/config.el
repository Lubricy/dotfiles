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
  (map!
   :map vterm-mode-map
   :g "M-v" #'vterm-yank)

  (map!
   :map vterm-copy-mode-map
   :g "M-v" (cmd!
             (vterm-yank))
   :n "p" (cmd!
           (vterm-yank))
   :v "<return>" (cmd!
                  (call-interactively #'vterm-copy-mode-done)
                  (vterm-yank))
   :n "<return>" #'evil-insert)
  (advice-add #'vterm-yank :after (cmd! (call-interactively #'evil-insert)))

  (add-hook! evil-insert-state-entry
    (when (bound-and-true-p vterm-copy-mode)
      (vterm-copy-mode -1))))
