;;; lang-go.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:


(use-package go-mode
  :ensure t
  :bind (:map go-mode-map
         ("C-c a" . go-test-current-project)
         ("C-c m" . go-test-current-file)
         ("C-c ." . go-test-current-test)
         ("C-c b" . go-run)
         ("C-h f" . godoc-at-point)
         ("C-c f" . gofmt))
  :config
  (add-hook 'go-mode-hook 'company-mode)
  (add-hook 'go-mode-hook 'go-eldoc-setup)

  ;; try to use goimports for formatting
  (let ((goimports (executable-find "goimports")))
         (when goimports
           (setq gofmt-command goimports)))
  (add-hook 'before-save-hook 'gofmt-before-save nil t)
  
  )

(use-package company-go
  :ensure t
  :defer t
  :init
  (with-eval-after-load "company"
    (add-to-list 'company-backends 'company-go)))


;; (use-package flycheck-gometalinter
;;   :ensure t
;;   :defer t
;;   :config
;;   (progn
;;     (flycheck-gometalinter-setup)))

(use-package go-eldoc
  :ensure t
  :defer t)

(use-package go-projectile
  :ensure t
  :defer t)

(provide 'lang-go)
;;; lang-go.el ends here
