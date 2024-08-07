(when (package! eaf :recipe (:host github
                             :repo "emacs-eaf/emacs-application-framework"
                             :files ("*.el" "*.py" "core" "app" "extension" "*.json")
                             :pre-build (("python" "install-eaf.py"
                                          "--install" "pdf-viewer" "browser" "terminal"
                                          "--ignore-sys-deps"))))

  (package! ctable :recipe (:host github :repo "kiwanami/emacs-ctable"))
  (package! deferred :recipe (:host github :repo "kiwanami/emacs-deferred"))
  (package! epc :recipe (:host github :repo "kiwanami/emacs-epc")))
