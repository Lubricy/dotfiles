;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here


;; I've swapped these keys on my keyboard
(setq x-meta-keysym         'alt
      x-alt-keysym          'meta
      mac-option-modifier   'alt
      mac-command-modifier  'meta
      mac-function-modifier 'control

      user-mail-address "lubricy@gmail.com"
      user-full-name    "Lubricy Fibber")

(load! "+local-config")
(load! "tweaks/init")

(provide 'config)
;;; config.el ends here
