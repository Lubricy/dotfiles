;; load the stock config.el from ~/.emacs.d/modules/lang/lean/
(load-file (expand-file-name "modules/lang/lean/config.el" doom-emacs-dir))

(when (modulep! :lang lean +lean4)
  (after! lean-mode
    (add-to-list 'auto-mode-alist
                 '("\\.lean\\'" .
                   (lambda ()
                     (if (locate-dominating-file (buffer-file-name) "lean-toolchain")
                         (lean4-mode)
                       (lean-mode)))))))

(use-package! lean4-mode
  :after lean-mode)
