(load-directory! "lang")

(after! lsp-mode
  (dolist (dir '("[/\\\\]\\.venv\\'"
                 "[/\\\\]\\..*_cache\\'"
                 "[/\\\\]\\.lean\\'"))
    (add-to-list 'lsp-file-watch-ignored-directories dir)))
