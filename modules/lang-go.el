;;; lang-go.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(defvar *go-use-lsp* t)

(use-package go-mode
  :ensure t
  :bind (:map go-mode-map
              ("C-h f" . godoc-at-point)
              ("C-c C-f" . gofmt))
  :config
  (font-lock-add-keywords
   'go-mode '(("\\<[\\+-]?[0-9]+\\(.[0-9]+\\)?\\>" 0 'font-lock-constant-face)))

  ;; try to use goimports for formatting
  (let ((goimports (executable-find "goimports")))
    (when goimports
      (setq gofmt-command goimports)))
  (add-hook 'before-save-hook 'gofmt-before-save nil t))


;; install command: go get -u github.com/sourcegraph/go-langserver
(use-package lsp-go
  :when *go-use-lsp*
  :ensure t
  :commands lsp-go-enable
  :hook (go-mode . lsp-go-enable))


(use-package go-eldoc
  :ensure t
  :defer t
  :init (add-hook 'go-mode-hook 'go-eldoc-setup))


(use-package company-go
  :unless *go-use-lsp*
  :after company-mode
  :defines company-backends
  :init (add-to-list 'company-backends 'company-go))


(use-package go-projectile
  :after projectile-mode
  :commands (go-projectile-mode go-projectile-switch-project)
  :hook ((go-mode . go-projectile-mode)
         (projectile-after-switch-project . go-projectile-switch-project)))


;; install: go get -u github.com/derekparker/delve/cmd/dlv
(use-package go-dlv
  :ensure t)


;; install: go get -u github.com/davidrjenni/reftools/cmd/fillstruct
(use-package go-fill-struct
  :ensure t)


;; install: go get -u github.com/josharian/impl
(use-package go-impl
  :ensure t)


;; install: go get -u golang.org/x/tools/cmd/gorename
(use-package go-rename
  :ensure t)


;; install: go get -u github.com/golang/lint/golint
(use-package golint
  :ensure t)


(use-package govet
  :ensure t)


;; install: go get -u github.com/fatih/gomodifytags
(use-package go-tag
  :ensure t
  :config (setq go-tag-args (list "-transform" "camelcase")))


;; install: go get -u github.com/cweill/gotests/...
(use-package gotest
  :bind (:map go-mode-map
              ("C-c C-t f" . go-test-current-file)
              ("C-c C-t p" . go-test-current-project)
              ("C-c C t t" . go-test-current-test)
              ("C-c x" . go-run))
  :config
  (use-package go-gen-test
    :bind (:map go-mode-map
                ("C-c C-t g" . go-gen-test-dwim))))


(provide 'lang-go)
;;; lang-go.el ends here
