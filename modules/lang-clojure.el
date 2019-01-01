;;; lang-clojure.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(use-package clojure-mode
  :ensure t
  :mode ("\\.boot\\'" . clojure-mode))


;; CIDER setup
(use-package cider
  :defer t
  :ensure t
  :commands (cider cider-connect cider-jack-in)
  :bind (:map clojure-mode-map ("C-c C-z" . cider-jack-in))
  :custom
  (cider-auto-select-error-buffer t)
  (cider-repl-pop-to-buffer-on-connect t)
  (cider-repl-use-clojure-font-lock t)
  (cider-repl-wrap-history t)
  (cider-repl-history-size 1000)
  (cider-show-error-buffer t)
  (nrepl-hide-special-buffers t)
  (nrepl-popup-stacktraces nil)
  :init
  (add-hook 'cider-repl-mode-hook 'superword-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook 'smartparens-mode)
  (add-hook 'cider-test-report-mode 'jcf-soft-wrap))


;; linter setup with flycheck
(use-package flycheck-clojure
  :ensure t
  :init (add-hook 'flycheck-mode-hook 'flycheck-clojure-setup))


;; refactoring with clj-refactor
;; refactoring help with C-c r h h
(use-package clj-refactor
  :ensure t
  :init (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  :config (cljr-add-keybindings-with-prefix "C-c C-r")
  :diminish clj-refactor-mode)


(provide 'lang-clojure)
;;; lang-clojure.el ends here
