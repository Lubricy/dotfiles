(after! org
  (add-to-list 'org-latex-packages-alist '("" "tikz-cd" t))
  (setq org-preview-latex-default-process 'dvisvgm))
