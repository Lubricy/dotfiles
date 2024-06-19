(use-package! sqlite)

(use-package! browser-hist
  :init
  (require 'embark) ; load Embark before the command (if you're using it)
  :config
  (setq browser-hist-default-browser 'chrome)
  :commands (browser-hist-search))
