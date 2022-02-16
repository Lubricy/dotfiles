(after! pdf-view
  (map! :map pdf-view-mode-map
        :n  "u"         #'pdf-view-scroll-down-or-previous-page
        :n  "d"         #'pdf-view-scroll-up-or-next-page)

  (add-hook! pdf-view-mode
    (pdf-view-fit-width-to-window)
    (pdf-view-themed-minor-mode 1)))

(after! org-noter
  (setq org-noter-always-create-frame t
        org-noter-notes-window-location 'horizontal-split
        org-noter-doc-split-fraction '(0.8 . 0.8))
  (map! :map org-noter-doc-mode-map
        :after pdf-view
        :n "q"   #'org-noter-kill-session
        :n "i"   #'org-noter-insert-note
        :n "I"   #'org-noter-insert-precise-note))


(use-package! pdf-continuous-scroll-mode
  :after pdf-view
  :when (featurep! +scroll)
  :config
  (defun pdf-continuous-next-half-page ()
    (interactive)
    (pdf-continuous-scroll-forward (+ (/  (cdr (nth (1- (book-current-page)) (book-image-sizes))) 2)
                                      (* 2 (or book-page-vertical-margin pdf-view-image-relief)))))

  (defun pdf-continuous-previous-half-page ()
    (interactive)
    (pdf-continuous-scroll-backward (+ (/  (cdr (nth (1- (book-current-page)) (book-image-sizes))) 2)
                                       (* 2 (or book-page-vertical-margin pdf-view-image-relief)))))
  (setq pdf-continuous-suppress-introduction 1
        book-page-vertical-margin 0
        pdf-view-image-relief 1)
  (map! :map pdf-view-mode-map
        :n  "j"         #'pdf-continuous-scroll-forward
        :n  "<down>"    #'pdf-continuous-scroll-forward
        :n  "k"         #'pdf-continuous-scroll-backward
        :n  "<up>"      #'pdf-continuous-scroll-backward
        :n  "<mouse-5>" #'pdf-cs-mouse-scroll-forward
        :n  "<mouse-4>" #'pdf-cs-mouse-scroll-backward
        :n  "u"         #'pdf-continuous-previous-half-page
        :n  "d"         #'pdf-continuous-next-half-page))
