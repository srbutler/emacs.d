;;; lang-web-js.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(use-package web-mode
  :ensure t
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ;; ("\\.jsx?\\'" . web-mode)
         )

  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-indent-style 2
        web-mode-style-padding 1
        web-mode-script-padding 1
        web-mode-block-padding 0)

  ;; allows linting jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode))


;; emmet mode for efficient xml/html entry
(use-package emmet-mode
  :ensure t
  :diminish (emmet-mode . "emmet")
  :commands emmet-mode

  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-hook 'html-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook  'emmet-mode)
  (add-hook 'nxml-mode-hook 'emmet-mode)

  :config
  (setq emmet-indentation                2
        emmet-move-cursor-between-quotes t))


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
         ("\\.ejs\\'" . js2-mode)
         ("\\.pac\\'" . js2-mode)
         ("\\.jsx?\\'" . js2-jsx-mode))
   :init (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))
  :config
  (js2-imenu-extras-setup)
  
  (setq mode-name "JS2")
  
  (setq-default js-basic-indent 2
                js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil
                js2-basic-offset 2
                js2-auto-indent-p t
                js2-cleanup-whitespace t
                js2-enter-indents-newline t
                js2-indent-on-enter-key t
                js2-global-externs (list "window" "module" "require" "buster"
                                         "sinon" "assert" "refute" "setTimeout"
                                         "clearTimeout" "setInterval""clearInterval"
                                         "location" "__dirname" "console" "JSON"
                                         "jQuery" "$")) 
  
  ;; flycheck setup (prefer eslint)
  (setq flycheck-disabled-checkers
        (append flycheck-disabled-checkers
                '(javascript-jshint)))
  
  (add-hook 'js2-mode-hook 'subword-mode))


;; formatting/beatufication for HTML/CSS/JS
(use-package web-beautify
  :ensure t
  :defer t
  :config
  (eval-after-load 'js2-mode
    '(define-key js2-mode-map (kbd "C-c C-f") 'web-beautify-js))
  (eval-after-load 'json-mode
    '(define-key json-mode-map (kbd "C-c C-f") 'web-beautify-js))
  (eval-after-load 'sgml-mode
    '(define-key html-mode-map (kbd "C-c C-f") 'web-beautify-html))
  (eval-after-load 'css-mode
    '(define-key css-mode-map (kbd "C-c C-f") 'web-beautify-css)))


;; set up javascript refactoring, all prefixed to =C-c .=
(use-package js2-refactor
  :ensure t
  :commands js2-refactor-mode js2r-add-keybindings-with-prefix
  :init   (add-hook 'js2-mode-hook 'js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c ."))


;; JS autocompletion via company
(use-package tern
  :ensure t
  :commands tern-mode
  :diminish (tern-mode . "tern")
  :init
  (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
  ;; Setup Tern as an autocomplete source.
  (with-eval-after-load "company"
    (use-package company-tern
      :init (add-to-list 'company-backends 'company-tern))))


;; for js2-comint, to call node.js as a REPL
(use-package js-comint
  :ensure t
  :defer t
  :init
  (setq inferior-js-program-command "node")
  (setq inferior-js-program-arguments '("--interactive"))

  (add-hook 'js2-mode-hook '(lambda ()
                              (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                              (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
                              (local-set-key "\C-cb" 'js-send-buffer)
                              (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                              (local-set-key "\C-cl" 'js-load-file-and-go)
                              ))

  (setenv "NODE_NO_READLINE" "1")
  (setq inferior-js-mode-hook
        (lambda ()
          (ansi-color-for-comint-mode-on))))

(provide 'lang-web-js.el)
;;; lang-web.el ends here
