(use-package! eat
  :hook (eshell-load . eat-eshell-mode)
  :config
  ;; --- Settings ---
  (setq eat-kill-buffer-on-exit t)
  (setq eat-shell "pwsh.exe")
  (setq eat-enable-blinking-text nil)
  ;; (setq eat-term-name "xterm-256color")

  ;; --- Evil Integration ---
  (after! evil
    (set-evil-initial-state! 'eat-mode 'emacs))

  (defun +eat/launch (program arg display-buffer-fn)
    "Start a new Eat terminal emulator in a buffer.

PROGRAM and ARG is same as in `eat' and `eat-other-window'.
DISPLAY-BUFFER-FN is the function to display the buffer."
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
    "Start Eat in the current project's root directory.

Start a new Eat session, or switch to an already active session.
Return the buffer selected (or created).

With a non-numeric prefix ARG, create a new session.

With a numeric prefix ARG (like
\\[universal-argument] 42 \\[eat-project]), switch to the session with
that number, or create it if it doesn't already exist."
    (interactive "P")
    (require 'project)
    (let* ((default-directory (project-root (project-current t)))
           (eat-buffer-name (project-prefixed-buffer-name "eat-full")))
      (+eat/launch nil arg #'pop-to-buffer-same-window)
      ))

  (defun +eat/project-popup (&optional arg)
    "Start Eat in the current project root directory in another window.

Start a new Eat session, or switch to an already active session.
Return the buffer selected (or created).

With a non-numeric prefix ARG, create a new session.

With a numeric prefix ARG (like
\\[universal-argument] 42 \\[eat-project]), switch to the session with
that number, or create it if it doesn't already exist."
    (interactive "P")
    (require 'project)
    (let* ((default-directory (project-root (project-current t)))
           (eat-buffer-name (project-prefixed-buffer-name "eat")))
      (+eat/launch nil arg #'pop-to-buffer)))
  
  ;; --- Keybindings ---
  (map! :leader
        (:prefix ("o" . "open")
         :desc "Terminal popup"      "t" #'+eat/project-popup
         :desc "Terminal fullscreen" "T" #'+eat/project))

  ;; --- Popup Rules ---
  (set-popup-rule! "^.*-eat\\*"
    :side 'bottom
    :size 0.35
    :select t
    :quit t
    :ttl nil)
  )
