(use-package! evil-string-inflection
  :after evil
  :config
  (map!
   :map evil-normal-state-map
   "]~" #'evil-operator-string-inflection))

(use-package! ox-ipynb
  :after ox)
