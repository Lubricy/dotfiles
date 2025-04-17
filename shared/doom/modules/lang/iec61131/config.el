(use-package! iec61131-mode
  :config
  (define-derived-mode
    st-mode iec61131-mode
    "Structured Text (IEC 61131-3)"
    "A major mode for editing Structured Text files after IEC 61131-3")
  (add-to-list 'auto-mode-alist '("\\.st\\'" . st-mode))
  (add-to-list 'auto-mode-alist '("\\.stl\\'" . st-mode)))
