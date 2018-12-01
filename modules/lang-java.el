;;; lang-java.el --- Summary:
;;
;;; Commentary:
;;  Follow instructions in: https://github.com/emacs-lsp/lsp-java
;;  This setup assumes that the lsp server is installed in .emacs.d
;;
;;; Code:

(use-package lsp-java
  :ensure t
  :requires (lsp-ui-flycheck lsp-ui-sideline)
  :config
  (add-hook 'java-mode-hook 'lsp-java-enable)
  ;; (add-hook 'java-mode-hook (lambda () (lsp-ui-flycheck-enable t)))
  ;; (add-hook 'java-mode-hook 'lsp-ui-mode)
  ;; (setq lsp-java--workspace-folders (list (error "XXX Specify your projects here")))
  )


(provide 'lang-java)
;;; lang-java.el ends here
