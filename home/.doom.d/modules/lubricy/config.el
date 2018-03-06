;;; private/lubricy/config.el -*- lexical-binding: t; -*-

(when (featurep! :feature evil)
  (load! +environments)  ; my environments
  (load! +bindings)  ; my key bindings
  (load! +commands)) ; my custom ex commands

(def-package! docker-tramp
              :after tramp
              :config
              (require 'docker-tramp-compat))

(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2))

