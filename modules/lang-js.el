;;; config-js.el -- Summary
;;
;;; Commentary:
;;
;;; Code:

(use-package js2-mode
  :ensure t
  :mode (("\\.js$"  . js2-mode)
         ("\\.es6$" . js2-mode))
  :config
  (setq mode-name "JS2")
  (setq js-basic-indent 2)
  (setq js2-basic-offset 2)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-auto-indent-p t)
  (setq js2-cleanup-whitespace t)
  (setq js2-enter-indents-newline t)
  (setq js2-indent-on-enter-key t)
  (setq js2-global-externs (list "window" "module" "require" "buster"
                            "sinon" "assert" "refute" "setTimeout"
                            "clearTimeout" "setInterval""clearInterval"
                            "location" "__dirname" "console" "JSON"
                            "jQuery" "$"))
  (js2-imenu-extras-setup)
  (add-hook 'js2-mode-hook 'subword-mode)

  ;; flycheck setup (prefer eslint)
  (setq flycheck-disabled-checkers
        (append flycheck-disabled-checkers
                '(javascript-jshint))))


(use-package rjsx-mode
  :ensure t
  :mode (("\\.jsx$" . rjsx-mode))
  :config
  (setq mode-name "RJSX")
  (setq js-basic-indent 2)
  (setq js2-basic-offset 2)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-auto-indent-p t)
  (setq js2-cleanup-whitespace t)
  (setq js2-enter-indents-newline t)
  (setq js2-indent-on-enter-key t)
  (setq js2-global-externs (list "window" "module" "require" "buster"
                            "sinon" "assert" "refute" "setTimeout"
                            "clearTimeout" "setInterval""clearInterval"
                            "location" "__dirname" "console" "JSON"
                            "jQuery" "$"))

  (js2-imenu-extras-setup)
  (add-hook 'rjsx-mode-hook 'subword-mode)

  ;; flycheck setup (prefer eslint)
  (setq flycheck-disabled-checkers
        (append flycheck-disabled-checkers
                '(javascript-jshint))))


;; for typescript
(use-package typescript-mode
  :ensure t
  :mode ("\\.tsx?$" . typescript-mode))


;; for JS/TS autocompletion
(use-package tide
  :ensure t
  :defer t
  :init
  (add-hook 'js2-mode-hook
            #'(lambda ()
                (tide-mode)
                (flycheck-add-next-checker 'javascript-eslint
                                           'javascript-tide
                                           'append)))
  (add-hook 'rjsx-mode-hook
            #'(lambda ()
                (tide-mode)
                (flycheck-add-next-checker 'javascript-eslint
                                           'jsx-tide
                                           'append)))
  (add-hook 'typescript-mode-hook 'tide-mode)
  :config (tide-setup))


;; javascript refactoring
(use-package js2-refactor
  :ensure t
  :diminish (js2-refactor-mode . "js2r")
  :init
  (add-hook 'js2-mode-hook 'js2-refactor-mode)
  (add-hook 'rjsx-mode-hook 'js2-refactor-mode)
  :config
  (with-eval-after-load 'js2-mode
    (bind-key "C-k" 'js2r-kill js2-mode-map))
  (with-eval-after-load 'rjsx-mode
    (bind-key "C-k" 'js2r-kill rjsx-mode-map))

  (js2r-add-keybindings-with-prefix "C-c r"))


(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode))


;; formatting/beatufication for HTML/CSS/JS
(use-package web-beautify
  :ensure t
  :after (:any js2-mode rjsx-mode))


;; JS autocompletion
(use-package tern
  :disabled t
  :ensure t
  :if (executable-find "tern")
  :commands tern-mode
  :diminish (tern-mode . "tern")
  :init (add-hook 'js2-mode-hook 'tern-mode))


;; connect tern with company
(use-package company-tern
  :disabled t
  :after (company tern-mode)
  :init
  (add-hook 'js2-mode-hook
            #'(lambda ()
                (set (make-local-variable 'company-backends)
                     '((company-tern company-files company-yasnippet))))))


;; prettify javascript files on save
(use-package prettier-js
  :ensure t
  :if (executable-find "prettier")
  :after (:any js2-mode json-mode)
  :hook ((js2-mode . prettier-js-mode))
  :init
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'json-mode-hook 'prettier-js-mode))


;; REPL/dev environment
(use-package indium
  :ensure t
  :after js2-mode
  :bind (:map js2-mode-map
              ("C-c C-l" . indium-eval-buffer))
  :hook ((js2-mode . indium-interaction-mode)))


(provide 'lang-js.el)
;;; lang-js.el ends here
