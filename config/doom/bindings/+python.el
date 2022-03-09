(after! poetry
  (defvar poetry-mode-map (make-sparse-keymap))
  (map! :localleader
        :prefix ("p" . "poetry")
        :map python-mode-map
        :desc "poetry add"        "a"  #'poetry-add-dep
        :desc "poetry add..."     "A"  #'poetry-add
        :desc "poetry add dev"    "d" #'poetry-add-dev-dep
        :desc "poetry install"    "i" #'poetry-install-install
        :desc "poetry install..." "I" #'poetry-install
        :desc "poetry add dev"    "b" #'poetry-build
        :desc "poetry lock"       "l" #'poetry-lock
        :desc "poetry remove"     "r" #'poetry-remove
        :desc "poetry shell"      "s" #'poetry-shell
        :desc "poetry update"     "u" #'poetry-update))
