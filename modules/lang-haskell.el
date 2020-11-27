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
  :hook ((haskell-mode . haskell-doc-mode)
         (haskell-mode . haskell-decl-scan-mode)
         (haskell-mode . haskell-indentation-mode))
  :init
  (setq haskell-process-type 'stack-ghci
        haskell-process-suggest-add-package nil
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-ask-also-kill-buffers nil
        haskell-stylish-on-save t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t
        haskell-interactive-popup-errors nil))


(use-package intero
  :ensure t
  :hook (haskell-mode . intero-mode))


(use-package hindent
  :when (executable-find "hindent")
  :ensure t
  :bind (:map haskell-mode-map ("C-c i" . hindent-reformat-buffer))
  :hook haskell-mode)


(use-package company-ghci
  :disabled
  :ensure t
  :after haskell
  :config
  (push
   '(company-ghci :with company-yasnippet :with company-dabbrev)
   company-backends))


(provide 'lang-haskell)
;;; lang-haskell.el ends here
