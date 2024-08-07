(after! org
  (add-to-list 'plantuml-jar-args "-tsvg")
  (setq! plantuml-default-exec-mode 'jar))
