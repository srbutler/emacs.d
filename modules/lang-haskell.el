;;; lang-haskell.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(use-package haskell-mode
  :ensure t
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.cabal$". haskell-mode))
  :init
  (add-hook 'haskell-mode-hook 'subword-mode)
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)

  :config
  (setq 
        haskell-compile-cabal-build-command "stack build"
        haskell-font-lock-symbols nil
        ;; haskell-interactive-mode-scroll-to-bottom t
        ;; haskell-interactive-popup-errors nil
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        ;; haskell-process-load-or-reload-prompt t
        ;; haskell-process-log t
        ;; haskell-process-suggest-remove-import-lines  t
        haskell-process-type 'stack-ghci
        ;; haskell-process-type 'auto
        haskell-stylish-on-save t
        ;; haskell-completions-complete-operators t
        ;; haskell-notify-p t
        ;; haskell-doc-show-reserved nil
        ;; haskell-indent-spaces 4
        ;; haskell-indent-offset 4
        ;; haskell-doc-show-global-types t
      )

  )

;; (use-package ghc
;;   :ensure t
;;   :init (add-hook 'haskell-mode-hook 'ghc-init))

;; (use-package company-ghc
;;   :ensure t
;;   :init (add-to-list 'company-backends 'company-ghc))

;; (use-package flycheck-haskell
;;   :disabled t
;;   :commands flycheck-haskell-setup)

(use-package flycheck-stack
  :ensure t
  :init
  (setq flycheck-check-syntax-automatically
        '(save idle-change new-line mode-enabled))

  (defun haskell-mode-flycheck-stack ()
    (flycheck-select-checker 'stack)
    (flycheck-mode))
  (add-hook 'haskell-mode-hook 'haskell-mode-flycheck-stack))

(provide 'lang-haskell)
;;; lang-haskell.el ends here
