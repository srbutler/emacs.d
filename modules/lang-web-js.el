;;; lang-web-js.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-indent-style 2)
  (web-mode-style-padding 1)
  (web-mode-script-padding 1)
  (web-mode-block-padding 0)

  ;; allows linting jsx files
  :config (flycheck-add-mode 'javascript-eslint 'web-mode))


;; emmet mode for efficient xml/html entry
(use-package emmet-mode
  :ensure t
  :diminish (emmet-mode . "emmet")
  :commands emmet-mode
  :hook (sgml-mode css-mode html-mode web-mode nxml-mode)
  :custom
  (emmet-indentation 2)
  (emmet-move-cursor-between-quotes t))


;; set up skewer browser REPL
(use-package skewer-mode
  :ensure t
  :commands skewer-mode run-skewer
  :diminish (skewer-mode . "skewer")
  :disabled t
  :config (skewer-setup))


(use-package js2-mode
  :ensure t
  :mode (("\\.jsx?$"  . js2-mode)
         ("\\.es6\\'" . js2-mode)
         ("\\.jsx?\\'" . js2-jsx-mode))
  :interpreter ("node" . js2-mode)
  :custom
  (mode-name "JS2")
  (js-basic-indent 2)
  (js2-mode-show-parse-errors nil)
  (js2-mode-show-strict-warnings nil)
  (js2-basic-offset 2)
  (js2-auto-indent-p t)
  (js2-cleanup-whitespace t)
  (js2-enter-indents-newline t)
  (js2-indent-on-enter-key t)
  (js2-global-externs (list "window" "module" "require" "buster"
                            "sinon" "assert" "refute" "setTimeout"
                            "clearTimeout" "setInterval""clearInterval"
                            "location" "__dirname" "console" "JSON"
                            "jQuery" "$"))
  :config
  (js2-imenu-extras-setup)

  ;; flycheck setup (prefer eslint)
  (setq flycheck-disabled-checkers
        (append flycheck-disabled-checkers
                '(javascript-jshint)))

  (add-hook 'js2-mode-hook 'subword-mode)
  (add-hook 'js2-jsx-mode-hook 'subword-mode)
  )


;; formatting/beatufication for HTML/CSS/JS
(use-package web-beautify
  :ensure t
  :after (:any js2-mode web-mode css-mode))


;; set up javascript refactoring, all prefixed to =C-c r=
(use-package js2-refactor
  :ensure t
  :diminish js2-refactor-mode
  :init (add-hook 'js2-mode-hook 'js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c r"))


;; JS autocompletion via company
(use-package tern
  :ensure t
  :commands tern-mode
  :diminish (tern-mode . "tern")
  :hook js2-mode)


(use-package company-tern
  :after (company tern-mode)
  :init (add-to-list 'company-backends 'company-tern))


(use-package nodejs-repl
  :ensure t
  :after js2-mode
  :bind (:map js2-mode-map
         ("C-x C-e" . nodejs-repl-send-last-expression)
         ("C-c C-j" . nodejs-repl-send-line)
         ("C-c C-r" . nodejs-repl-send-region)
         ("C-c C-l" . nodejs-repl-load-file)
         ("C-c C-z" . nodejs-repl-switch-to-repl)))


(provide 'lang-web-js.el)
;;; lang-web.el ends here


