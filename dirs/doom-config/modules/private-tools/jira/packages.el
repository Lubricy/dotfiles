;; -*- no-byte-compile: t; -*-
;;; tools/jira/packages.el

(package! dash-functional)
(package! ejira
  :recipe (:host github
           :repo "nyyManni/ejira"))
