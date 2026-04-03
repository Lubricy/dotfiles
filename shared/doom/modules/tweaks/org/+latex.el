(after! org
  (add-to-list 'org-latex-packages-alist '("" "tikz-cd" t))
  (add-to-list 'org-latex-packages-alist '("" "lmodern"))
  (setq org-preview-latex-default-process 'dvisvgm))
