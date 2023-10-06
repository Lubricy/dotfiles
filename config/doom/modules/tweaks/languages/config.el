(load-directory! "lang")

(after! lsp-mode
  (dolist (dir '("[/\\\\]\\.venv\\'"
                 "[/\\\\]\\..*_cache\\'"))
    (add-to-list 'lsp-file-watch-ignored-directories dir)))
