;;; lang-web.el --- Summary:
;;
;;; Commentary:
;;  Much of this config was taken from:
;;    https://github.com/seagle0128/.emacs.d
;;
;;; Code:

(use-package web-mode
  :ensure t
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :commands flycheck-add-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-indent-style 2)
  (web-mode-style-padding 1)
  (web-mode-script-padding 1)
  (web-mode-block-padding 0)
  :config
  ;; allows linting jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode))


(use-package nxml-mode
  :ensure nil
  :mode (("\\.xml\\'" . nxml-mode)
         ("\\.pom$"   . nxml-mode))
  :custom
  (nxml-child-indent 4)
  (nxml-attribute-indent 5)
  (nxml-auto-insert-xml-declaration-flag nil)
  (nxml-bind-meta-tab-to-complete-flag t)
  (nxml-slash-auto-complete-flag t)
  :config
  (add-hook 'nxml-mode-hook 'smartparens-mode))


(use-package css-mode
  :ensure nil
  :init (setq css-indent-offset 2))


;; SCSS mode
(use-package scss-mode
  :ensure t
  :defer t
  :init
  ;; Disable complilation on save
  (setq scss-compile-at-save nil))


;; New `less-css-mode' in Emacs 26
(unless (fboundp 'less-css-mode)
  (use-package less-css-mode :ensure t))


;; CSS eldoc
(use-package css-eldoc
  :commands turn-on-css-eldoc
  :hook ((css-mode scss-mode less-css-mode) . turn-on-css-eldoc))


;; CSS, LESS, and SCSS/SASS support for lsp-mode using vscode-css-languageserver-bin
(use-package lsp-css
  :ensure t
  :ensure-system-package
  (css-languageserver . "npm i -g vscode-css-languageserver-bin")
  :commands (lsp-css-enable lsp-less-enable lsp-sass-enable lsp-scss-enable)
  :hook ((css-mode . lsp-css-enable)
         (less-mode . lsp-less-enable)
         (sass-mode . lsp-sass-enable)
         (scss-mode . lsp-scss-enable)))


;; HTML support for lsp-mode using vscode-html-languageserver-bin
(use-package lsp-html
  :ensure t
  :ensure-system-package
  (html-languageserver . "npm i -g vscode-html-languageserver-bin")
  :commands lsp-html-enable
  :hook ((html-mode . lsp-html-enable)
         (web-mode . lsp-html-enable)))


;; emmet mode for efficient xml/html entry
(use-package emmet-mode
  :ensure t
  :diminish (emmet-mode . "emmet")
  :commands emmet-mode
  :hook (sgml-mode css-mode html-mode web-mode nxml-mode)
  :custom
  (emmet-indentation 2)
  (emmet-move-cursor-between-quotes t))


;; Live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode
  :ensure t
  :diminish skewer-mode
  :hook ((js2-mode . skewer-mode)
         (css-mode . skewer-css-mode)
         (web-mode . skewer-html-mode)
         (html-mode . skewer-html-mode))
  :init
  ;; diminish
  (with-eval-after-load 'skewer-css
    (diminish 'skewer-css-mode))
  (with-eval-after-load 'skewer-html
    (diminish 'skewer-html-mode)))


(provide 'lang-web.el)
;;; lang-web.el ends here
