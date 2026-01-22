;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(if IS-MAC
    (add-to-list 'default-frame-alist '(undecorated-round . t)))

(setq doom-theme 'doom-dracula
      doom-themes-padded-modeline t)

;; (setq doom-symbol-font
;;       (font-spec :family "Symbols Nerd Font"))
;; I've swapped these keys on my keyboard
(setq x-super-keysym        'meta
      x-alt-keysym          'alt
      mac-option-modifier   'alt
      mac-command-modifier  'meta
      mac-function-modifier 'control

      user-mail-address "lubricy@gmail.com"
      user-full-name    "Lubricy Fibber")



;; (run-with-idle-timer 5 nil #'lubricy/clock-in-idle)

;; (defmacro load-directory! (dir &optional root)
;;   `(let* ((root (or ,root (dir!)))
;;           (path (expand-file-name ,dir root)))
;;      (mapc (lambda (file) (load! file)) (directory-files-recursively path "\\.el$"))))

(load-directory! "bindings")
(load-directory! "tweaks")
(load-directory! "private")

(provide 'config)
;;; config.el ends here
