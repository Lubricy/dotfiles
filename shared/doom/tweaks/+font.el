(if (featurep :system 'windows)
    (progn
      (w32-find-non-USB-fonts)
      (setq! doom-font (font-spec :family "CaskaydiaMono NF" :size 12.0)))
  (setq doom-font
        (font-spec :family "CaskaydiaCove NFM" :size 11.0)))
