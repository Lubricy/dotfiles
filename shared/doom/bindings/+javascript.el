(after! npm-mode
  (map! :localleader
        (:map npm-mode-keymap
          "p" npm-mode-command-keymap)
        (:after js2-mode
          :map js2-mode-map
          :prefix ("p" . "npm"))))
