;;; lang-go.el --- Summary:
;;
;;; Commentary:
;;  - install: go get -u github.com/golang/lint/golint
;;  - install: go get -u -v golang.org/x/tools/cmd/guru
;;  - install: go get -u honnef.co/go/tools/... (megacheck)
;;  - install: go get -u golang.org/x/tools/cmd/goimports
;;  - install: go get -u github.com/rogpeppe/godef
;;
;; For fedora, install via DNF:
;; - golang-x-tools-gorename.x86_64
;; - golang-godoc.x86_64
;; - golang-x-tools-goimports.x86_64
;; - golang-x-tools-guru.x86_64
;; - golang-x-tools-gopls.x86_64
;;
;; Then (use -insecure in toolbox):
;; go get -u github.com/motemen/gore/cmd/gore
;;
;;; Code:

(defvar *go-use-lsp* t)

(use-package go-mode
  :ensure t
  :bind (:map go-mode-map
              ("C-h f" . godoc-at-point)
              ("C-c C-f" . gofmt))
  :init
  (when *go-use-lsp* (add-hook 'go-mode-hook 'lsp))

  :config
  (font-lock-add-keywords
   'go-mode '(("\\<[\\+-]?[0-9]+\\(.[0-9]+\\)?\\>" 0 'font-lock-constant-face)))

  ;; try to use goimports for formatting
  (let ((goimports (executable-find "goimports")))
    (when goimports
      (setq gofmt-command goimports)))
  (add-hook 'before-save-hook 'gofmt-before-save nil t))


(use-package go-tag
  :ensure t
  :bind (:map go-mode-map ("C-c C-r t" . go-tag-add))
  :config (setq go-tag-args (list "-transform" "camelcase")))


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
