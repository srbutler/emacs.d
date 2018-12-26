;;; lang-java.el --- Summary:
;;
;;; Commentary:
;;  Follow instructions in: https://github.com/emacs-lsp/lsp-java
;;
;;; Code:

(use-package lsp-java
  :ensure t
  :after lsp
  :config (add-hook 'java-mode-hook 'lsp))

(provide 'lang-java)
;;; lang-java.el ends here
