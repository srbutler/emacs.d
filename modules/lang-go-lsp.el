;;; lang-go.el --- Summary:
;;
;;; Commentary:
;;    Borrowed a lot from:
;;    https://github.com/seagle0128/.emacs.d
;;
;;; Code:


(use-package go-mode
  :ensure t
  :ensure-system-package
  ((gocode . "go get -u github.com/mdempsky/gocode")
   (godef . "go get -u github.com/rogpeppe/godef")
   (goimports . "go get -u golang.org/x/tools/cmd/goimports"))
  :bind (:map go-mode-map
              ("C-h f" . godoc-at-point)
              ("C-c C-f" . gofmt))
  :config
  (font-lock-add-keywords
   'go-mode '(("\\<[\\+-]?[0-9]+\\(.[0-9]+\\)?\\>" 0 'font-lock-constant-face)))

  (use-package go-eldoc
    :disabled t
    :ensure t
    :defer t
    :init (add-hook 'go-mode-hook 'go-eldoc-setup))

  ;; try to use goimports for formatting
  (let ((goimports (executable-find "goimports")))
    (when goimports
      (setq gofmt-command goimports)))
  (add-hook 'before-save-hook 'gofmt-before-save nil t))

(use-package lsp-go
  :ensure t
  :ensure-system-package
  (go-langserver . "go get -u github.com/sourcegraph/go-langserver")
  :commands lsp-go-enable
  :hook (go-mode . lsp-go-enable))


(use-package go-dlv
  :ensure t
  :ensure-system-package
  (dlv . "go get -u github.com/derekparker/delve/cmd/dlv"))


(use-package go-fill-struct
  :ensure t
  :ensure-system-package
  (fillstruct . "go get -u github.com/davidrjenni/reftools/cmd/fillstruct"))


(use-package go-impl
  :ensure t
  :ensure-system-package
  (impl . "go get -u github.com/josharian/impl"))


(use-package go-rename
  :ensure t
  :ensure-system-package
  (gorename . "go get -u golang.org/x/tools/cmd/gorename"))


(use-package golint
  :ensure t
  :ensure-system-package
  (golint . "go get -u github.com/golang/lint/golint"))


(use-package govet
  :ensure t)


(use-package go-tag
  :ensure t
  :ensure-system-package
  (gomodifytags . "go get -u github.com/fatih/gomodifytags")
  :config (setq go-tag-args (list "-transform" "camelcase")))


(use-package gotest
  :ensure-system-package (gotests . "go get -u github.com/cweill/gotests/...")
  :bind (:map go-mode-map
              ("C-c C-t f" . go-test-current-file)
              ("C-c C-t p" . go-test-current-project)
              ("C-c C t t" . go-test-current-test)
              ("C-c x" . go-run))
  :config
  (use-package go-gen-test
    :bind (:map go-mode-map
                ("C-c C-t g" . go-gen-test-dwim))))


(with-eval-after-load 'company
  (use-package company-go
    :disabled t
    :defines company-backends
    :functions company-backend-with-yas
    :init (add-to-list 'company-backends 'company-go)))

(with-eval-after-load 'projectile
  (use-package go-projectile
    :commands (go-projectile-mode go-projectile-switch-project)
    :hook ((go-mode . go-projectile-mode)
           (projectile-after-switch-project . go-projectile-switch-project))))


(provide 'lang-go)
;;; lang-go.el ends here
