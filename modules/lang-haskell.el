;;; lang-haskell.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:


(use-package haskell-mode
  :defer t
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.cabal$". haskell-mode))
  :bind (:map haskell-mode-map
              ("M-," . pop-tag-mark)
              ("C-," . haskell-move-nested-left)
              ("C-." . haskell-move-nested-right)
              ("C-c h" . haskell-hoogle)
              ("C-c C-c" . haskell-compile)
              ("C-c C-z" . haskell-interactive-switch)
              ("C-c v c" . haskell-cabal-visit-file))
  :init
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
  (add-hook 'haskell-mode-hook 'subword-mode)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)

  (setq haskell-process-type 'stack-ghci
        haskell-process-suggest-add-package nil
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-ask-also-kill-buffers nil
        haskell-stylish-on-save t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t
        haskell-interactive-popup-errors nil)

  :config

  ;; Smartparens clean up
  (add-hook 'smartparens-mode-hook
            (lambda ()
              (add-to-list 'sp-no-reindent-after-kill-modes #'haskell-mode)
              (require 'smartparens-haskell) ; should add {-# #-}
              (sp-local-pair 'haskell-mode "'" nil :actions nil)
              (sp-local-pair 'haskell-mode "\\(" nil :actions nil)
              (sp-local-pair 'interactive-haskell-mode "\\(" nil :actions nil)
              (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil))))


(use-package intero
  :ensure t
  :defer t
  :diminish (intero-mode . "intero")
  :init (add-hook 'haskell-mode-hook 'intero-mode))


(use-package hindent
  :ensure t
  :defer t
  :bind (:map haskell-mode-map ("C-c i" . hindent-reformat-buffer))
  :init
  (when (locate-library "hindent")
    (add-hook 'haskell-mode-hook #'hindent-mode)))


;; (use-package company-ghci
;;   :ensure t
;;   :after haskell
;;   :config
;;   (push
;;    '(company-ghci :with company-yasnippet :with company-dabbrev)
;;    company-backends))


(provide 'lang-haskell)
;;; lang-haskell.el ends here
