(package! corfu
  :recipe (:files (:defaults "extensions/*.el")))
(unpin! evil-collection)
(when (modulep!
       +orderless)
  (package! orderless))
(package! kind-icon)
(package! cape :recipe (:host github :repo "minad/cape" :branch "main"))
