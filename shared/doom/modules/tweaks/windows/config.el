(when (file-exists-p "/proc/sys/fs/binfmt_misc/WSLInterop") ; HACK: inside WSL
  ;; HACK: fix clipboard coding system
  (setq! x-select-request-type '(text/plain\;charset=utf-8)))

(when (featurep :system 'windows)
  (after! consult
    ;; HACK: Windows systems struggle with rapid process spawning,
    ;; for example, when invoking ripgrep to search through a project.
    (setq! consult-async-input-throttle 0.5)))
