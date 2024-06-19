(after! haskell
  (map! :localleader
        :map dante-mode-map
        :desc "restart dante server" "R"  #'dante-restart))
