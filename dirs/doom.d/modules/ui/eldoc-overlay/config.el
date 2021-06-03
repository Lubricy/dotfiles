;;; ui/eldoc-overlay/config.el -*- lexical-binding: t; -*-

;;
;;; Packages



;; (use-package! pos-tip
;;   :defer t)

;; (defun my-eldoc-display-message (format-string &rest args)
;;   "Display eldoc message near point."
;;   (when format-string
;;     (popup-tip (apply 'format format-string args) &key :nowait 't)))
;; (setq eldoc-message-function #'my-eldoc-display-message)

;; cache candidates for better performance
