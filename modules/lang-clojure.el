;;; package --- Summary:
;; lang-clojure.el
;;
;;; Commentary:
;;
;;; Code:

;; setup clojure-mode, with symbol prettification
(use-package clojure-mode
  :mode (("\\.edn$" . clojure-mode)
         ("\\.cljc$" . clojure-mode))
  :ensure t
  :init
  
  (defconst clojure--prettify-symbols-alist
    '(("fn"   . ?λ)
      ("__"   . ?⁈)))

  (add-hook 'clojure-mode-hook 'global-prettify-symbols-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook 'subword-mode)

  ;; (eval-after-load 'clojure-mode
  ;;   '(use-package clojure-mode-extra-font-locking
  ;;      :ensure t
  ;;      :init (require 'clojure-mode-extra-font-locking)))
  )

;; defines a function for better REPL interaction
(defun cider-send-and-evaluate-sexp ()
  "Sends the s-expression located before the point or the active
   region to the REPL and evaluates it. Then the Clojure buffer
   is activated as if nothing happened."
  (interactive)
  (if (not (region-active-p))
      (cider-insert-last-sexp-in-repl)
    (cider-insert-in-repl
     (buffer-substring (region-beginning) (region-end)) nil))
  (cider-switch-to-repl-buffer)
  (cider-repl-closing-return)
  (cider-switch-to-last-clojure-buffer)
  (message ""))

;; CIDER (the REPL) setup
(use-package cider
  :defer t
  :ensure t
  :commands (cider cider-connect cider-jack-in)
  :init
  (setq cider-auto-select-error-buffer t
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-use-clojure-font-lock t
        cider-repl-wrap-history t
        cider-repl-history-size 1000
        cider-show-error-buffer t
        nrepl-hide-special-buffers t
        ;; Stop error buffer from popping up while working in
        ;; buffers other than the REPL:
        nrepl-popup-stacktraces nil)

  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-mode-hook 'company-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'superword-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'cider-test-report-mode 'jcf-soft-wrap)

  :bind ("C-c C-v" . cider-send-and-evaluate-sexp)

  ;; (bind-key "C-x C-e" 'cider-eval-last-sexp clojure-mode-map)
  ;; (bind-key "C-c C-v" 'cider-send-and-evaluate-sexp)
  )


;; linter setup with flycheck
(use-package flycheck-clojure
  :defer t
  :ensure t
  :init (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (use-package flycheck
    :config
    (flycheck-clojure-setup)))

;; refactoring with clj-refactor
;; refactoring help with C-c . h h
(use-package clj-refactor
  :defer t
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  :config
  ;; Configure the Clojure Refactoring prefix:
  (cljr-add-keybindings-with-prefix "C-c .")
  :diminish clj-refactor-mode)

;;; lang-clojure.el ends here