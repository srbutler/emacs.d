;;; lang-web.el --- Summary:
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


(provide 'lang-web.el)
;;; lang-web.el ends here
