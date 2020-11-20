;; -*- no-byte-compile: t; -*-
;;; lang/org/packages.el

(load! "lang/org/packages" doom-modules-dir)

(package! ob-spark-shell
  :recipe (:host github
           :repo "pepijn/ob-spark-shell"))
