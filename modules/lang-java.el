;;; lang-java.el --- Summary:
;;
;;; Commentary:
;;  Follow instructions in: https://github.com/emacs-lsp/lsp-java
;;  This setup assumes that the lsp server is installed in .emacs.d
;;
;;; Code:

;; (use-package lsp-javacomp
;;   :ensure t
;;   :after lsp-mode
;;   :commands lsp-javacomp-enable
;;   :init
;;   (add-hook 'java-mode-hook 'lsp-javacomp-enable)
;;   (add-hook 'java-mode-hook
;;             (lambda ()
;;               (set (make-variable-buffer-local 'company-backends) '(company-lsp))))
;;   :config (lsp-javacomp-install-server))

(use-package lsp-javacomp
  :ensure t
  :disabled t
  :after lsp-mode
  :commands lsp-javacomp-enable
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              ;; Load company-lsp before enabling lsp-javacomp, so that function
              ;; parameter snippet works.
              (require 'company-lsp)
              (lsp-javacomp-enable)
              ;; Use company-lsp as the company completion backend
              (set (make-variable-buffer-local 'company-backends) '(company-lsp))
              ;; Optional company-mode settings
              (set (make-variable-buffer-local 'company-idle-delay) 0.1)
              (set (make-variable-buffer-local 'company-minimum-prefix-length) 1)))
  ;; Optional, make sure JavaComp is installed. See below.
  :config
  (lsp-javacomp-install-server))


(provide 'lang-java)
;;; lang-java.el ends here
