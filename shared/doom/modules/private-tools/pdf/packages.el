(package! pdf-tools :recipe
  (:host github
   :repo "dalanicolai/pdf-tools"
   :branch "pdf-roll"
   :files ("lisp/*.el"
           "README"
           ("build" "Makefile")
           ("build" "server")
           (:exclude "lisp/tablist.el" "lisp/tablist-filter.el"))))

(package! image-roll :recipe
  (:host github
   :repo "dalanicolai/image-roll.el"))
