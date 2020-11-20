;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/haskell/doctor.el

(load! "lang/haskell/doctor" doom-modules-dir)

(when (featurep! +eglot)
  (unless (executable-find "ghcide")
    (warn! "Couldn't find ghcide, eglot may have issues")))
