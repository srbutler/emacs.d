;;; config-js-lsp.el -- Summary
;;
;;; Commentary:
;;
;;; Code:


(use-package js2-mode
  :ensure t
  :defines flycheck-javascript-eslint-executable
  :ensure-system-package (eslint_d . "npm install -g eslint_d")
  :mode ("\\.js\\'"  . js2-mode)
  :interpreter ("node" . js2-mode)
  :bind (:map js2-mode-map ("M-." . nil))  ;; don't conflict with xref
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode))
  :config
  (setq js2-basic-offset 2)

  (with-eval-after-load 'flycheck
    (if (executable-find "eslint")
        (setq js2-mode-show-strict-warnings nil)
        (setq flycheck-javascript-eslint-executable "eslint"))))


;; for jsx
(use-package rjsx-mode
  :ensure t
  :defines flycheck-javascript-eslint-executable
  :ensure-system-package (eslint_d . "npm install -g eslint_d")
  :mode (("\\.jsx\\'" . rjsx-mode)
         ("components\\/.*\\.js\\'" . rjsx-mode))
  :interpreter ("node" . rjsx-mode)
  :hook ((rjsx-mode . js2-imenu-extras-mode)
         (rjsx-mode . js2-highlight-unused-variables-mode))
  :config
  (setq js2-basic-offset 2)

  (with-eval-after-load 'flycheck
    (if (executable-find "eslint")
        (setq js2-mode-show-strict-warnings nil)
        (setq flycheck-javascript-eslint-executable "eslint"))))


;; for typescript
(use-package typescript-mode
  :ensure t
  :mode ("\\.tsx?$" . typescript-mode))


;; from https://github.com/seagle0128/.emacs.d
(use-package js2-refactor
  :ensure t
  :hook (js2-mode . js2-refactor-mode)
  :diminish (js2-refactor-mode . "js2r")
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (with-eval-after-load 'js2-mode
    (bind-key "C-k" 'js2r-kill js2-mode-map)))


;; from https://github.com/seagle0128/.emacs.d
(use-package lsp-javascript-typescript
  :ensure t
  :ensure-system-package
  (javascript-typescript-langserver . "npm i -g javascript-typescript-langserver")
  :commands lsp-javascript-typescript-enable
  :hook ((typescript-mode js2-mode rjsx-mode) . lsp-javascript-typescript-enable))


(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)
         ("\\.eslintrc\\'" . json-mode)))


;; auto-formatter
(use-package prettier-js
  :ensure t
  :bind (:map js2-mode-map ("C-c C-f" . prettier-js)
         :map rjsx-mode-map ("C-c C-f" . prettier-js)
         :map typescript-mode-map ("C-c C-f" . prettier-js))
  :init
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode))


;; REPL/dev environment
(use-package indium
  :ensure t
  :after js2-mode
  :bind (:map js2-mode-map
              ("C-c C-l" . indium-eval-buffer))
  :hook ((js2-mode . indium-interaction-mode)))


(provide 'lang-js-lsp.el)
;;; lang-js-lsp.el ends here
