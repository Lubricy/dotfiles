;;; lang/org/contrib/babel.el -*- lexical-binding: t; -*-
;;;###if (featurep! +spark)

(use-package! ob-spark-shell
  :defer t
  :init
  (setq ob-ipython-resources-dir ".ob-ipython-resrc")

  (add-hook! '+org-babel-load-functions
    (defun +org-babel-load-ipython-h (lang)
      (and (string-prefix-p "spark-" (symbol-name lang))
           (require 'ob-spark-shell nil t))))

  (after! org-src
    (add-to-list 'org-src-lang-modes '("spark-shell" . scala))
    (add-to-list 'org-src-lang-modes '("spark-pyshell" . python)))
  :config
  (set-popup-rules!
    '(("\\*ob-ipython.*"
       :slot 2 :side right :size 100 :height 0.2
       :select nil :quit nil :ttl nil)
      ("^ \\*Python"
       :slot 0 :side right :size 100
       :select nil :quit nil :ttl nil)))

  ;; advices for remote kernel and org-src-edit
  (advice-add #'ob-ipython--create-repl :override #'+org-ob-ipython-create-repl-a)
  (advice-add #'org-babel-edit-prep:ipython :override #'+org-babel-edit-prep:ipython-a)
  (advice-add #'org-babel-execute:ipython :before #'+org-babel-execute:ipython-a)
  (advice-add #'org-babel-ipython-initiate-session :override #'+org-ob-ipython-initiate-session-a)

  ;; ipython has its own async keyword, disable ipython in ob-async.
  (after! ob-async
    (add-to-list 'ob-async-no-async-languages-alist "spark-shell")))
