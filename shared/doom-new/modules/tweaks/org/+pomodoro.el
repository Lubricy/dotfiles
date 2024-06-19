;;;###if (modulep! +pomodoro)

(after! org-pomodoro
  (setq org-pomodoro-short-break-sound "~/Music/material_product_sounds/wav/01 Hero Sounds/hero_simple-celebration-01.wav")
  (setq org-pomodoro-long-break-sound "~/Music/material_product_sounds/wav/01 Hero Sounds/hero_simple-celebration-03.wav")
  (setq org-pomodoro-finished-sound "~/Music/material_product_sounds/wav/01 Hero Sounds/hero_simple-celebration-02.wav")
  (setq org-pomodoro-length 45)
  (setq org-pomodoro-short-break-length 2)
  (setq org-pomodoro-long-break-length 10))
