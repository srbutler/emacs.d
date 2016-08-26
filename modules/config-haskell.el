;;; package --- Summary:
;; python-setup.el
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package haskell-mode
  :ensure t
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.cabal$". haskell-mode))
 
  :init
  (add-hook 'haskell-mode-hook 'subword-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'wrap-region-mode)
  (add-hook 'haskell-mode-hook 'electric-pair-mode)
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  
  :config
  (setq haskell-process-type 'stack-ghci
        haskell-process-suggest-remove-import-lines  t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t
        haskell-stylish-on-save t
        ;; haskell-font-lock-symbols t
        )

  (bind-keys :map haskell-mode-map
             ;; Indent the below lines on columns after the current column.
             ("C-,"     . haskell-move-nested-left)
             ;; Same as above but backwards.
             ("C-."     . haskell-move-nested-right)
             ("C-c C-." . haskell-mode-format-imports)
             ;; Switch to the REPL.
             ("C-c C-z" . haskell-interactive-switch)
             ;; “Bring” the REPL, hiding all other windows apart from
             ;; the source and the REPL.
             ("C-`"     . haskell-interactive-bring)
             ;; Load the current file (and make a session if not
             ;; already made).
             ("C-c C-l" .  haskell-process-load-or-reload)
             ;; Get the type and info of the symbol at point, print it
             ;; in the message buffer.
             ("C-c C-t" . haskell-process-do-type)
             ("C-c C-i" . haskell-process-do-info)
             ;; Build the Cabal project.
             ("C-c C-c" . haskell-process-cabal-build)
             ("C-c C-k" . haskell-interactive-mode-clear)
             ("C-c c"   . haskell-process-cabal)
             )

  ;; Indent the below lines on columns after the current column.
  (define-key haskell-mode-map (kbd "C-<right>")
    (lambda ()
      (interactive)
      (haskell-move-nested 1)))
  ;; Same as above but backwards.
  (define-key haskell-mode-map (kbd "C-<left>")
    (lambda ()
      (interactive)
      (haskell-move-nested -1)))
            

  ;; Useful to have these keybindings for .cabal files, too.
  (defun haskell-cabal-hook ()
    (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
    (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
    (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
    (define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch))
  )

(use-package flycheck-haskell
  :commands flycheck-haskell-setup)

(provide 'config-haskell)
;;; config-haskell.el ends here
