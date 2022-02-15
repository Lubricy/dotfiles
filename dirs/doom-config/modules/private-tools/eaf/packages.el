(when (package! eaf :recipe (:host github
                             :repo "emacs-eaf/emacs-application-framework"
                             :files ("*.el" "*.py" "app" "core" "extention")
                             :build (:not compile)))

  (package! ctable :recipe (:host github :repo "kiwanami/emacs-ctable"))
  (package! deferred :recipe (:host github :repo "kiwanami/emacs-deferred"))
  (package! epc :recipe (:host github :repo "kiwanami/emacs-epc")))
