;;; lang-web.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(defvar *web-use-lsp* nil)

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
  (web-mode-block-padding 0))


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
  (add-hook 'nxml-mode-hook 'smartparens-mode)
  (add-hook 'nxml-mode-hook 'emmet-mode))


(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode))


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
(use-package less-css-mode
  :unless (fboundp 'less-css-mode)
  :ensure t)


;; emmet mode for efficient xml/html entry
(use-package emmet-mode
  :ensure t
  :diminish (emmet-mode . "emmet")
  :commands emmet-mode
  :hook (sgml-mode css-mode html-mode web-mode nxml-mode)
  :custom
  (emmet-indentation 2)
  (emmet-move-cursor-between-quotes t))


;; formatting/beatufication for HTML/CSS/JS
(use-package web-beautify
  :ensure t
  :init
  (with-eval-after-load 'json-mode
    (bind-key "C-c C-f" #'web-beautify-js json-mode-map))
  (with-eval-after-load 'web-mode
    (bind-key "C-c C-f" #'web-beautify-html web-mode-map))
  (with-eval-after-load 'sgml-mode
    (bind-key "C-c C-f" #'web-beautify-html html-mode-map))
  (with-eval-after-load 'css-mode
    (bind-key "C-c C-f" #'web-beautify-css css-mode-map))
  :config
  ;; Set indent size to 2
  (setq web-beautify-args '("-s" "2" "-f" "-")))


;; CSS eldoc
(use-package css-eldoc
  :commands turn-on-css-eldoc
  :hook ((css-mode scss-mode less-css-mode) . turn-on-css-eldoc))


;; CSS, LESS, and SCSS/SASS support for lsp-mode using vscode-css-languageserver-bin
;; install: npm i -g vscode-css-languageserver-bin
(use-package lsp-css
  :unless *web-use-lsp*
  :ensure t
  :hook ((css-mode . lsp-css-enable)
         (less-mode . lsp-less-enable)
         (sass-mode . lsp-sass-enable)
         (scss-mode . lsp-scss-enable)))


;; HTML support for lsp-mode using vscode-html-languageserver-bin
;; install: npm i -g vscode-html-languageserver-bin
(use-package lsp-html
  :unless *web-use-lsp*
  :ensure t
  :hook ((html-mode . lsp-html-enable)
         (web-mode . lsp-html-enable)))


(provide 'lang-web)
;;; lang-web.el ends here
