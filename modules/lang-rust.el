;;; lang-rust.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:


(use-package rustic
  :ensure t
  :hook (rustic-mode . lsp)
  :config
  (setq ;; rustic-lsp-server 'rls
        rustic-format-trigger 'on-save)

  (push 'rustic-clippy flycheck-checkers))


(provide 'lang-rust)
;;; lang-rust.el ends here
