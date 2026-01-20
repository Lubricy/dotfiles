(package! eat
  :pin "windows-hack"
  :recipe (:host codeberg
           :repo "thearcticcat/emacs-eat"
           :branch "windows-hack"
           :files ("*.el" ("term" "term/*.el") "*.texi"
                   "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el"))))
