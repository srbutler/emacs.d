;;; lang-scala.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(use-package scala-mode
  :straight t
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
  :straight t)


(provide 'lang-scala)
;;; lang-scala.el ends here
