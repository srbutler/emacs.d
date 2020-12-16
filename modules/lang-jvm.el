;;; lang-java.el --- Summary:
;;
;;; Commentary:
;;  Follow instructions in: https://github.com/emacs-lsp/lsp-java
;;
;;; Code:

(use-package lsp-java
  :ensure t
  :hook (java-mode . (lambda () (require 'lsp-java) (lsp))))


(use-package kotlin-mode
  :ensure t)


(use-package clojure-mode
  :ensure t
  :mode ("\\.boot\\'" . clojure-mode))


(use-package scala-mode
  :ensure t)


(provide 'lang-java)
;;; lang-java.el ends here
