(use-package! pdf-tools)

(use-package! image-roll
  :after pdf-view
  :when (modulep! +scroll)
  :config
  (add-hook! pdf-view-mode
    (pdf-view-roll-minor-mode 1))
  (add-hook! pdf-view-mode
    (pdf-view-roll-minor-mode 1))
  (add-hook 'image-roll-after-change-page-hook #'pdf-view-change-page-hook)
  (defvar +pdf-view-half-page-repeat 10)
  (defun pdf-continuous-next-half-page ()
    (interactive)
    (pdf-view-next-line-or-next-page +pdf-view-half-page-repeat))

  (defun pdf-continuous-previous-half-page ()
    (interactive)
    (pdf-view-previous-line-or-previous-page +pdf-view-half-page-repeat))
  (map! :map pdf-view-mode-map
        :n  "u"         #'pdf-continuous-previous-half-page
        :n  "d"         #'pdf-continuous-next-half-page))
