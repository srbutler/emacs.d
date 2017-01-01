;;; lang-go.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:


(use-package go-mode
  :ensure t
  :bind (("C-c a" . go-test-current-project)
         ("C-c m" . go-test-current-file)
         ("C-c ." . go-test-current-test)
         ("C-c b" . go-run)
         ("C-h f" . godoc-at-point)
         ("C-c C-f" . gofmt))
  :config
  (add-hook 'go-mode-hook 'company-mode)
  (add-hook 'go-mode-hook 'go-eldoc-setup)

  (setq gofmt-command "goimports"))

(use-package company-go
  :ensure t
  :init
  (with-eval-after-load "company"
    (add-to-list 'company-backends 'company-go)))

(use-package go-eldoc
  :ensure t)

(provide 'lang-go)
;;; lang-go.el ends here
