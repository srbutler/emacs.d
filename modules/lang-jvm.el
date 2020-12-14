;;; lang-java.el --- Summary:
;;
;;; Commentary:
;;  Follow instructions in: https://github.com/emacs-lsp/lsp-java
;;
;;; Code:

(use-package lsp-java
  :ensure t
  :after lsp
  :hook (java-mode . lsp))


(use-package clojure-mode
  :ensure t
  :mode ("\\.boot\\'" . clojure-mode))


(use-package scala-mode
  :ensure t
  :pin melpa
  :config
  ;; better newline handling in comments
  (defun scala-mode-newline-comments ()
    "Custom newline appropriate for `scala-mode'."
    ;; shouldn't this be in a post-insert hook?
    (interactive)
    (newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment))
  (bind-key "RET" 'scala-mode-newline-comments scala-mode-map)

  ;; skips headers, goes to go when opening a file
  (scala-mode:goto-start-of-code))


(provide 'lang-java)
;;; lang-java.el ends here
