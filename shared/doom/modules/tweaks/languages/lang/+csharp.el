(after! csharp-mode
  (add-hook! csharp-mode
    (setq-local vimish-fold-marks '("#region" . "#endregion"))
    (vimish-fold-mode t)))
(after! lsp-mode
  (setq lsp-disabled-clients '(csharp-roslyn omnisharp))
  (setq lsp-csharp-omnisharp-enable-decompilation-support t)
  ;; Prevent lsp-mode from watching files in the Temp directory to save CPU
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]AppData[/\\\\]Local[/\\\\]Temp[/\\\\]" t))
