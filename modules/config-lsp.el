;;; config-lsp.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:


;; make language server protocol services available
(use-package lsp-mode
  :ensure t
  :defer t
  :bind (("M-." . xref-find-definitions)
         ("M-?" . xref-find-references)
         ("C-c r" . lsp-rename)))


;; flycheck support and code previews/lenses
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook ((lsp-mode . lsp-ui-mode)))


;; link lsp output with company
(use-package company-lsp
  :ensure t
  :after (lsp-mode company)
  :config
  (push 'company-lsp company-backends)

  (setq company-lsp-async t
        company-lsp-cache-candidates t
        company-lsp-enable-recompletion t
        company-lsp-enable-snippet t))


;; let LSP work with imenu
(use-package lsp-imenu
  :ensure lsp-mode
  :hook ((lsp-after-open . lsp-enable-imenu)))


;; simpler automatic backend for lsp servers
;; see list of automatic backends here: https://github.com/joaotavora/eglot
(use-package eglot
  :disabled t
  :ensure t
  :defer t
  :bind (:map eglot-mode-map
              ("C-c e e" . eglot)
              ("C-c e c" . eglot-reconnect)
              ("C-c e s" . eglot-shutdown)
              ("C-c e r" . eglot-rename)

              ("C-c e a" . eglot-code-actions)
              ("C-c e h" . eglot-help-at-point)

              ("C-c C-f" . eglot-format)
              ("M-." . xref-find-definitions))
  :init
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'haskell-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure))


(provide 'config-lsp)
;;; config-lsp.el ends here
