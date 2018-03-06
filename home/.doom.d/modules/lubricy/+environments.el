;;; private/lubricy/+environments.el -*- lexical-binding: t; -*-

(after! tramp
  (add-to-list 'tramp-remote-process-environment "http_proxy=http://proxy-dln-1.fmr.com:8000")
  (add-to-list 'tramp-remote-process-environment "https_proxy=http://proxy-dln-1.fmr.com:8000"))
