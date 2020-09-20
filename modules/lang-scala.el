;;; lang-scala.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

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


(use-package sbt-mode
  :ensure t
  :pin melpa)


(provide 'lang-scala)
;;; lang-scala.el ends here
