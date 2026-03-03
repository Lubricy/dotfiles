(map! :leader
      (:when (modulep! :private-tools eat)
        (:prefix-map ("o" . "open")
         :desc "Terminal popup"      "t" #'+eat/project-popup
         :desc "Terminal fullscreen" "T" #'+eat/project)))

(use-package! eat
  ;; 1. Tell use-package these commands exist so it creates autoload stubs.
  ;;    When these are called, the package loads.
  :commands (+eat/project +eat/project-popup)

  ;; 2. Bind keys in :init. This runs immediately on startup.
  ;;    Since we used :commands above, these keys bind to the autoload stubs.
  :config
  ;; --- Settings ---
  (setq eat-kill-buffer-on-exit t)
  (setq eat-shell "pwsh.exe")
  (setq eat-enable-blinking-text nil)

  ;; --- Evil Integration ---
  (after! evil
    (set-evil-initial-state! 'eat-mode 'insert))

  ;; --- Custom Functions ---
  (defun +eat/launch (program arg display-buffer-fn)
    "Start a new Eat terminal emulator in a buffer."
    (let* ((program (or program (or explicit-shell-file-name
                                    (getenv "ESHELL")
                                    shell-file-name)))
           (args
            (cond
             ((eq system-type 'windows-nt)
              `("pwsh.exe" nil ("-NoExit")))
             (t
              `("/usr/bin/env" nil (list "sh" "-c" ,program)))))
           (buffer
            (cond
             ((numberp arg)
              (get-buffer-create (format "%s<%d>" eat-buffer-name arg)))
             (arg
              (generate-new-buffer eat-buffer-name))
             (t
              (get-buffer-create eat-buffer-name)))))
      (with-current-buffer buffer
        (unless (eq major-mode #'eat-mode)
          (eat-mode))
        (funcall display-buffer-fn buffer)
        (unless (and eat-terminal
                     (eat-term-parameter eat-terminal 'eat--process))
          (apply #'eat-exec buffer (buffer-name) args))
        buffer)))

  (defun +eat/project (&optional arg)
    "Start Eat in the current project's root directory."
    (interactive "P")
    (require 'project)
    (let* ((default-directory (project-root (project-current t)))
           (eat-buffer-name (project-prefixed-buffer-name "eat-full")))
      (+eat/launch nil arg #'pop-to-buffer-same-window)))

  (defun +eat/project-popup (&optional arg)
    "Start Eat in the current project root directory in another window."
    (interactive "P")
    (require 'project)
    (let* ((default-directory (project-root (project-current t)))
           (eat-buffer-name (project-prefixed-buffer-name "eat")))
      (+eat/launch nil arg #'pop-to-buffer)))

  ;; --- Popup Rules ---
  (set-popup-rule! "^.*-eat\\*"
    :side 'bottom
    :size 0.35
    :select t
    :quit t
    :ttl nil)
  )
