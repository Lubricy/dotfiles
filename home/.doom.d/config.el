;;; private/lubricy/config.el -*- lexical-binding: t; -*-

(load! +environments)  ; my environments
(load! +bindings)      ; my key bindings

(def-package! docker-tramp
              :after tramp
              :config
              (require 'docker-tramp-compat))

(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2))

