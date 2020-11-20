;;; lang/org/config.el -*- lexical-binding: t; -*-

(load! "lang/org/config" doom-modules-dir)

(cond ((featurep! +spark)
       (load! "+spark")))
