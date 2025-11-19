(after! csharp-mode
  (add-hook! csharp-mode
    (setq-local vimish-fold-marks '("#region" . "#endregion"))
    (vimish-fold-mode t)))
