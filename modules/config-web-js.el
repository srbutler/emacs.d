;;; config-web-js.el --- Summary:
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
         ("\\.html?\\'" . web-mode))

  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-indent-style 2
        web-mode-style-padding 1
        web-mode-script-padding 1
        web-mode-block-padding 0))

;; emmet mode for efficient xml/html entry
(use-package emmet-mode
  :ensure t
  :commands emmet-mode

  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-hook 'html-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook  'emmet-mode)

  :config
  (setq emmet-indentation                2
        emmet-move-cursor-between-quotes t))

;; set up skewer browser REPL
(use-package skewer-mode
  :ensure t
  :commands skewer-mode run-skewer
  :disabled t
  :config (skewer-setup))


(use-package js2-mode
  :ensure t
  :mode (("\\.jsx?$"  . js2-mode)
         ("\\.es6\\'" . js2-mode)
         ("\\.ejs\\'" . js2-mode)
         ("\\.pac\\'" . js2-mode))
  ;; :interpreter node
  :commands js2-mode
  :config
  (setq js-basic-indent 2
        mode-name "JS2")
  
  (setq-default js2-basic-offset 2
                js2-auto-indent-p t
                js2-cleanup-whitespace t
                js2-enter-indents-newline t
                js2-indent-on-enter-key t
                js2-global-externs (list "window" "module" "require" "buster"
                                         "sinon" "assert" "refute" "setTimeout"
                                         "clearTimeout" "setInterval""clearInterval"
                                         "location" "__dirname" "console" "JSON"
                                         "jQuery" "$"))

  ;; (setq-local electric-layout-rules '((?\; . after)))    ;; prelude
  ;; (js2-imenu-extras-mode +1)    ;; prelude
  (add-hook 'js2-mode-hook 'subword-mode))


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

(provide 'config-web-js.el)
;;; config-web.el ends here
