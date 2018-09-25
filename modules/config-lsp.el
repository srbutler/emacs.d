;;; config-lsp.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:


;; make language server protocol services available
(use-package lsp-mode
  :ensure t
  :defer t
  :config (counsel-gtags-mode -1)

  (defun restart-lsp-server ()
    "Restart LSP server."
    (interactive)
    (lsp-restart-workspace)
    (revert-buffer t t)
    (message "LSP server restarted.")))


;; flycheck support and code previews/lenses
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook ((lsp-mode . lsp-ui-mode))
  :bind (:map lsp-ui-mode-map
              ;; use the peak functions instead of jumps
              ("M-." . lsp-ui-peek-find-definitions)
              ("M-?" . lsp-ui-peek-find-references)
              ("C-c C-l c" . lsp-capabilities)
              ("C-c C-l d" . lsp-ui-doc-enable)
              ("C-c C-l h" . lsp-describe-thing-at-point)
              ("C-c C-l r" . lsp-rename)
              ("C-c C-l s" . lsp-ui-sideline-toggle-symbols-info)
              ("C-c C-l w" . restart-lsp-server)
              :map lsp-ui-peek-mode-map
              ("C-j" . lsp-ui-peek--goto-xref))
  :config
  (setq lsp-ui-sideline-delay 0.5
        lsp-ui-sideline-ignore-duplicate t))


;; link lsp output with company
(use-package company-lsp
  :ensure t
  :after (lsp-mode company)
  :config
  (push 'company-lsp company-backends)

  (setq company-lsp-async t
        company-lsp-cache-candidates nil
        company-lsp-enable-recompletion t
        company-lsp-enable-snippet t))


;; let LSP work with imenu
(use-package lsp-imenu
  :ensure lsp-mode
  :hook ((lsp-after-open . lsp-enable-imenu)))


(provide 'config-lsp)
;;; config-lsp.el ends here
