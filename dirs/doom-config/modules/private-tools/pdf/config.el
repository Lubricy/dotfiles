(use-package! pdf-continuous-scroll-mode
  :after pdf-view
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-continuous-scroll-mode)
  (map! :map pdf-view-mode-map
        :n  "j"         #'pdf-continuous-scroll-forward
        :n  "<down>"    #'pdf-continuous-scroll-forward
        :n  "k"         #'pdf-continuous-scroll-backward
        :n  "<up>"      #'pdf-continuous-scroll-backward
        :n  "<mouse-5>" #'pdf-continuous-scroll-forward
        :n  "<mouse-4>" #'pdf-continuous-scroll-backward
        :n  "u"         #'pdf-continuous-previous-page
        :n  "d"         #'pdf-continuous-next-page
        :n  "c"         #'pdf-continuous-scroll-mode
        ))
(after! org-noter
  (map! org-noter-doc-mode-map
        :n "i" #'org-noter-insert-note
        :n "I" #'org-noter-insert-precise-note))
