;;; config-local.el -- Summary
;;
;;; Commentary:
;;
;;; Code:

;; use tramp to connect to toolkit machines
(use-package toolkit-tramp
  :load-path "~/.emacs.d/vendor/toolkit")


;; major mode for comdb2 CSC2 files
(use-package csc-mode
  :load-path "~/.emacs.d/vendor/comdb2-emacs/"
  :mode ("\\.csc2\\'" . csc-mode))


(use-package lark-mode
  :load-path "~/.emacs.d/vendor/lark-mode/"
  :mode (("\\.lark\\'" . lark-mode)
         ("\\.ebnf\\'" . lark-mode)))


;; This uses the BbgProtocolHandler set up by the terminal
;; bbg: links should be supported
(with-eval-after-load 'org-mode
  (defun org-bbg-open (bbfunc)
    "Launch the Bloomberg Terminal with the given FUNC."
    (browse-url (concat "bbg://screens/" bbfunc)))
  (org-add-link-type "bbg" 'org-bbg-open))


(provide 'config-local)
;;; config-local.el ends here
