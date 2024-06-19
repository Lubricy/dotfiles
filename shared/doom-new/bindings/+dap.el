(defhydra dap-hydra (:color pink :hint nil :foreign-keys run)
  "
^Control^                ^Toggle^                 ^Breakpoints^            ^Eval
^^^^^^^----------------------------------------------------------------------------------------------
_s_: Next                _ts_: Session            _b_ : Toggle              _e_: Eval thing at point
_i_: Step in             _tt_: Thread             _C_ : Set condition       _E_: Eval region
_o_: Step out            _tf_: Stack frame        _H_ : Set hit count       _p_: Eval
_c_: Continue            _tl_: List locals        _L_ : Set log message     _w_: Add  watch
_r_: Restart             _tS_: List sessions      _tb_: List breakpoints   _W_: Remove watch
_u_: Up stack frame      _Q_ : Disconnect
_d_: Down stack frame

"
  ("s" dap-next)
  ("i" dap-step-in)
  ("I" dap-step-in)
  ("o" dap-step-out)
  ("O" dap-step-out)
  ("c" dap-continue)
  ("r" dap-restart-frame)
  ("R" dap-debug-restart)
  ("u" dap-up-stack-frame)
  ("d" dap-down-stack-frame)
  ("Q" dap-disconnect :color red)
  ("ts" dap-switch-session)
  ("tt" dap-switch-thread)
  ("tf" dap-switch-stack-frame)
  ("tl" dap-ui-locals)
  ("tb" dap-ui-breakpoints)
  ("tS" dap-ui-sessions)
  ("b" dap-breakpoint-toggle)
  ("C" dap-breakpoint-condition)
  ("H" dap-breakpoint-hit-condition)
  ("L" dap-breakpoint-log-message)
  ("e" dap-eval-thing-at-point)
  ("E" dap-eval-region)
  ("p" dap-eval)
  ("w" dap-ui-expressions-add)
  ("W" dap-ui-expressions-remove)
  ("q" nil "quit" :color blue))

(after! dap-mode
  (dap-register-debug-template "Node Attach"
                               (list :type "node"
                                     :request "attach"))
  (add-hook! dap-ui-sessions-mode #'dap-hydra))
