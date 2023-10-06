;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

 ;; Place your private configuration here

(if IS-MAC
    (add-to-list 'default-frame-alist '(undecorated-round . t)))

(setq doom-theme 'doom-dracula
      doom-themes-padded-modeline t)

;; I've swapped these keys on my keyboard
(setq x-super-keysym        'meta
      x-alt-keysym          'alt
      mac-option-modifier   'alt
      mac-command-modifier  'meta
      mac-function-modifier 'control

      user-mail-address "lubricy@gmail.com"
      user-full-name    "Lubricy Fibber")


(load-directory! "bindings")
(load-directory! "private")

(provide 'config)
;;; config.el ends here
