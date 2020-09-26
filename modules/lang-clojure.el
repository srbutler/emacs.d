;;; lang-clojure.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(use-package clojure-mode
  :straight t
  :mode ("\\.boot\\'" . clojure-mode))


;; CIDER setup
(use-package cider
  :defer t
  :straight t
  :commands (cider cider-connect cider-jack-in)
  :bind (:map clojure-mode-map ("C-c C-z" . cider-jack-in))
  :hook ((cider-repl-mode . superword-mode)
         (cider-repl-mode . company-mode)
         (cider-repl-mode . rainbow-delimiters-mode)
         (cider-repl-mode . smartparens-mode)
         (cider-test-report . jcf-soft-wrap))
  :custom
  (cider-auto-select-error-buffer t)
  (cider-repl-pop-to-buffer-on-connect t)
  (cider-repl-use-clojure-font-lock t)
  (cider-repl-wrap-history t)
  (cider-repl-history-size 1000)
  (cider-show-error-buffer t)
  (nrepl-hide-special-buffers t)
  (nrepl-popup-stacktraces nil))


;; linter setup with flycheck
(use-package flycheck-clojure
  :straight t
  :hook (flycheck-mode . flycheck-clojure-setup))


;; refactoring with clj-refactor
;; refactoring help with C-c r h h
(use-package clj-refactor
  :straight t
  :hook clojure-mode
  :config (cljr-add-keybindings-with-prefix "C-c C-r")
  :diminish clj-refactor-mode)


(provide 'lang-clojure)
;;; lang-clojure.el ends here
