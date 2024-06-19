(package! browser-hist :recipe (:host github :repo "agzam/browser-hist.el"))
(unless (and (fboundp 'sqlite-available-p)
             (sqlite-available-p))
  (package! sqlite))
