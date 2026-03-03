;; --- Keybindings ---
(map! :leader
      (:when (modulep! :private-tools agent-shell)
        (:prefix ("o" . "open")
         :desc "Coding Agent"      "c" #'agent-shell)))

(use-package! agent-shell
  :commands (agent-shell agent-shell-opencode-start-agent)
  :config
  ;; --- Popup Rules ---
  (set-popup-rule! "^.* Agent .*$"
    :side 'bottom
    :size 0.35
    :select t
    :quit t
    :ttl nil))
