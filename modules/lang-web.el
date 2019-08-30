;;; lang-web.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(defvar *web-use-lsp* nil)
;; install: npm install -g vscode-css-languageserver-bin
;; install: npm install -g vscode-html-languageserver-bin

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
  :init
  (when *web-use-lsp*
    (add-hook 'web-mode-hook 'lsp)
    (add-hook 'html-mode-hook 'lsp))
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
         ("\\.pom\\'" . nxml-mode)
         ("\\.xsd\\'" . nxml-mode)
         ("\\.bml\\'" . nxml-mode)
         ("\\.rsd\\'" . nxml-mode))
  :custom
  (nxml-child-indent 2)
  (nxml-attribute-indent 2)
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
  :mode ("\\.rasi\\'" . css-mode)
  :init
  (setq css-indent-offset 2)
  (when *web-use-lsp* (add-hook 'css-mode-hook 'lsp)))


;; SCSS mode
(use-package scss-mode
  :ensure t
  :defer t
  :init (when *web-use-lsp* (add-hook 'css-mode-hook 'lsp)))


;; New `less-css-mode' in Emacs 26
(use-package less-css-mode
  :unless (fboundp 'less-css-mode)
  :ensure t
  :init (when *web-use-lsp* (add-hook 'less-css-mode-hook 'lsp)))


;; CSS eldoc
(use-package css-eldoc
  :ensure t
  :commands turn-on-css-eldoc
  :hook ((css-mode scss-mode less-css-mode) . turn-on-css-eldoc))



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


(provide 'lang-web)
;;; lang-web.el ends here
