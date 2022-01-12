;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)


;;; FIXME https://github.com/org-roam/org-roam/issues/1934
(package! org :pin "73875939a8b5545ac53a86ec467239f510d14de8")
(package! jq-mode)
(package! es-mode)

(when IS-MAC
  (package! org-mac-link))
