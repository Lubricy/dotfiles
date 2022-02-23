(after! csv-mode
  (add-hook! csv-mode
    (call-interactively #'csv-align-fields)))
