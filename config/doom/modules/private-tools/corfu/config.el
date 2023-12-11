;; Corfu completion module


;; Reset lsp-completion provider
(add-hook! 'doom-init-modules-hook
  (after! lsp-mode
    (setq lsp-completion-provider :none)))

;; Pad before lsp modeline error info
(add-hook! 'lsp-mode-hook
  (setf (caadr
         (assq 'global-mode-string mode-line-misc-info))
        " "))

;; Set orderless filtering for LSP-mode completions
;; (add-hook! 'lsp-completion-mode-hook
;;   (setf (alist-get 'lsp-capf completion-category-defaults) '((styles . (orderless)))))

;; Set bindings
(map!
 (:prefix "C-x"
  :i "C-k" #'cape-dict
  :i "C-f" #'cape-file
  :i "C-o" #'completion-at-point
  :i "s" #'cape-ispell
  :i "C-n" #'cape-keyword
  :i "C-s" #'dabbrev-completion))

;; Fallback cleanly to consult in TUI
(setq-default completion-in-region-function #'consult-completion-in-region)

(use-package! corfu
  :custom
  (corfu-separator ?\s)       ;; Orderless field separator
  (corfu-preview-current nil) ;; Disable current candidate preview
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-on-exact-match nil)
  (corfu-quit-no-match 'separator)
  ;; (corfu-preselect-first t)
  (corfu-popupinfo-delay 1.0)
  :hook
  ((doom-first-buffer . global-corfu-mode)
   (doom-first-buffer . corfu-popupinfo-mode))
  :bind (:map corfu-map
              ("M-n" . corfu-popupinfo-scroll-down)
              ("M-p" . corfu-popupinfo-scroll-up)
              ("M-d" . corfu-popupinfo-toggle)
              ("SPC" . corfu-insert-separator)))


(use-package! orderless
  :when (modulep! +orderless)
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package! kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package! cape
  :defer t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(setq completion-cycle-threshold 1)

;; Enable indentation+completion using the TAB key.
;; Completion is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; Dirty hack to get c completion running
;; Discussion in https://github.com/minad/corfu/issues/34
(when (equal tab-always-indent 'complete)
  (map! :map c-mode-base-map
        :i [remap c-indent-line-or-region] #'completion-at-point))
