(when (file-exists-p "/proc/sys/fs/binfmt_misc/WSLInterop") ; HACK: inside WSL
  ;; HACK: fix clipboard coding system
  (setq! x-select-request-type '(text/plain\;charset=utf-8)))
