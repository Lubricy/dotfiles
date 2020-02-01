;;; lang/haskell/config.el -*- lexical-binding: t; -*-

(load! "lang/haskell/config" doom-modules-dir)

(cond ((featurep! +eglot)
       (load! "+eglot")))
