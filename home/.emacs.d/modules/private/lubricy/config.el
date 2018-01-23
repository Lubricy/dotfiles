;;; private/lubricy/config.el -*- lexical-binding: t; -*-

(when (featurep! :feature evil)
  (load! +bindings)  ; my key bindings
  (load! +commands)) ; my custom ex commands

(def-package! docker-tramp
              :after tramp
              :config
              (require 'docker-tramp-compat))
