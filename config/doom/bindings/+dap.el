(when (modulep! :tools debugger +lsp)
  (after! git-gutter-fringe
    (fringe-mode '8)))

(after! dap-mode
  (defhydra dap-hydra (:color pink :hint nil :foreign-keys run)
    "
^Stepping^                     ^Move^                     ^Switch^                 ^Breakpoints^            ^Eval
^^^^^^^-----------------------------------------------------------------------------------------------------------------------------
_<return>_: Next               _ss_: Next                 _se_: Session            _bb_: Toggle             _e_: Eval thing at point
   _I_    : Step in            _si_: Step in              _st_: Thread             _bd_: Delete             _r_: Eval region
   _O_    : Step out           _so_: Step out             _sf_: Stack frame        _ba_: Add                _E_: Eval
   _c_    : Continue           _su_: Up stack frame       _sl_: List locals        _bc_: Set condition      _A_: Add expression.
   _R_    : Restart frame      _sd_: Down stack frame     _sb_: List breakpoints   _bh_: Set hit count
   _u_    : Up stack frame                                _sS_: List sessions      _bl_: Set log message
   _d_    : Down stack frame
   _Q_    : Disconnect
"

    ("<return>" dap-next)
    ("ss" dap-next)
    ("I" dap-step-in)
    ("si" dap-step-in)
    ("O" dap-step-out)
    ("so" dap-step-out)
    ("c" dap-continue)
    ("R" dap-restart-frame)
    ("u" dap-up-stack-frame)
    ("d" dap-down-stack-frame)
    ("su" dap-up-stack-frame)
    ("sd" dap-down-stack-frame)
    ("se" dap-switch-session)
    ("st" dap-switch-thread)
    ("sf" dap-switch-stack-frame)
    ("sl" dap-ui-locals)
    ("sb" dap-ui-breakpoints)
    ("sS" dap-ui-sessions)
    ("bb" dap-breakpoint-toggle)
    ("ba" dap-breakpoint-add)
    ("bd" dap-breakpoint-delete)
    ("bc" dap-breakpoint-condition)
    ("bh" dap-breakpoint-hit-condition)
    ("bl" dap-breakpoint-log-message)
    ("E" dap-eval)
    ("A" dap-ui-expressions-add)
    ("r" dap-eval-region)
    ("e" dap-eval-thing-at-point)
    ("q" nil "quit" :color blue)
    ("Q" dap-disconnect :color red))
  (dap-register-debug-template "Node Attach"
                               (list :type "node"
                                     :request "attach"))
  (add-hook! dap-ui-sessions-mode #'dap-hydra))
