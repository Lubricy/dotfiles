(use-package! eaf
  :defer t
  :init (evil-set-initial-state 'eaf-mode 'emacs)
  :config
  (use-package! ctable)
  (use-package! deferred)
  (use-package! epc)
  (add-hook! evil-normal-state-entry
    (when (derived-mode-p 'eaf-mode)
      (map!
       :map eaf-mode-map
       "SPC" doom-leader-map
       "M-\\" doom-leader-map
       "<escape>" #'eaf-keyboard-quit)
      (setq emulation-mode-map-alists
            (delq 'evil-mode-map-alist emulation-mode-map-alists))))
  (add-to-list 'evil-insert-state-modes 'eaf-edit-mode))

(use-package! eaf-file-manager
  :after eaf
  :commands eaf-open)

(use-package! eaf-browser
  :after eaf
  :commands eaf-open-browser
  :config
  (eaf-bind-key clear_focus "<escape>" eaf-browser-keybinding)
  )

(use-package! eaf-pdf-viewer
  :after eaf
  :commands eaf-open
  :config
  (eaf-bind-key scroll_up_page "d" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "u" eaf-pdf-viewer-keybinding))

(use-package! eaf-image-viewer
  :after eaf
  :commands eaf-open)

(use-package! eaf-terminal
  :after eaf
  :commands eaf-open-terminal)
