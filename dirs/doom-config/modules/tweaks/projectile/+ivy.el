;;;###if (featurep! +ivy)
(after! ivy-prescient
  (setf (alist-get 'counsel-rg ivy-re-builders-alist) #'ivy--regex-plus))
