;;; config-js.el -- Summary
;;
;;; Commentary:
;;
;;; Code:

(defvar *js-use-lsp* nil)

;; from https://github.com/seagle0128/.emacs.d
;; Improved JavaScript editing mode
(use-package js2-mode
  :ensure t
  :defines flycheck-javascript-eslint-executable
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
      (setq flycheck-javascript-eslint-executable "eslint")))

  (use-package smartparens-javascript
    :after smartparens-mode))


;; for jsx
(use-package rjsx-mode
  :ensure t
  :defines flycheck-javascript-eslint-executable
  :mode (("\\.jsx\\'" . rjsx-mode)
         ("components\\/.*\\.js\\'" . rjsx-mode))
  :interpreter ("node" . rjsx-mode)
  :hook ((rjsx-mode . js2-imenu-extras-mode)
         (rjsx-mode . js2-highlight-unused-variables-mode))
  :bind (:map js2-mode-map ("M-." . nil))  ;; don't conflict with xref
  :config
  (setq js2-basic-offset 2)

  (with-eval-after-load 'flycheck
    (if (executable-find "eslint")
        (setq js2-mode-show-strict-warnings nil)
      (setq flycheck-javascript-eslint-executable "eslint")))

  (use-package smartparens-javascript
    :after smartparens-mode))


;; for typescript
(use-package typescript-mode
  :ensure t
  :mode ("\\.tsx?$" . typescript-mode)
  :config
  (use-package smartparens-javascript
    :after smartparens-mode))


;; from https://github.com/seagle0128/.emacs.d
(use-package js2-refactor
  :ensure t
  :hook (js2-mode . js2-refactor-mode)
  :diminish (js2-refactor-mode . "js2r")
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (with-eval-after-load 'js2-mode
    (bind-key "C-k" 'js2r-kill js2-mode-map)))


;; for JS/TS autocompletion
(use-package tide
  :unless *js-use-lsp*
  :ensure t
  :defer t
  :hook ((typescript-mode js2-mode rjsx-mode) . tide-setup))


;; install: npm i -g javascript-typescript-langserver
;; from https://github.com/seagle0128/.emacs.d
(use-package lsp-javascript-typescript
  :if *js-use-lsp*
  :ensure t
  :commands lsp-javascript-typescript-enable
  :hook ((typescript-mode js2-mode rjsx-mode) . lsp-javascript-typescript-enable))


;; auto-formatter
(use-package prettier-js
  :ensure t
  :bind (:map js2-mode-map
              ("C-c C-f" . prettier-js)
              :map rjsx-mode-map
              ("C-c C-f" . prettier-js))
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


(provide 'lang-js)
;;; lang-js.el ends here
