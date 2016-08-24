;;; package --- Summary:
;; python-setup.el
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package haskell-mode
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
        haskell-stylish-on-save t)

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
             ))


;; (use-package haskell-mode
;;   :ensure t
;;   :mode (("\\.hs\\'" . haskell-mode)
;;          ("\\.cabal$". haskell-mode)))

;;   ;; Haskell main editing mode key bindings.
;;   (defun haskell-hook ()
;;     ;; Use simple indentation.
;;     (turn-on-haskell-simple-indent)
;;     (define-key haskell-mode-map (kbd "<return>") 'haskell-simple-indent-newline-same-col)
;;     ;; (define-key haskell-mode-map (kbd "C-<return>") 'haskell-simple-indent-newline-indent)

;;     ;; Load the current file (and make a session if not already made).
;;     (define-key haskell-mode-map [?\C-c ?\C-l] 'haskell-process-load-file)
;;     (define-key haskell-mode-map [f5] 'haskell-process-load-file)

;;     ;; Switch to the REPL.
;;     (define-key haskell-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)
;;     ;; “Bring” the REPL, hiding all other windows apart from the source
;;     ;; and the REPL.
;;     (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)

;;     ;; Build the Cabal project.
;;     (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;;     ;; Interactively choose the Cabal command to run.
;;     (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

;;     ;; Get the type and info of the symbol at point, print it in the
;;     ;; message buffer.
;;     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
;;     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)

;;     ;; Contextually do clever things on the space key, in particular:
;;     ;;   1. Complete imports, letting you choose the module name.
;;     ;;   2. Show the type of the symbol after the space.
;;     (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

;;     ;; Jump to the imports. Keep tapping to jump between import
;;     ;; groups. C-u f8 to jump back again.
;;     (define-key haskell-mode-map [f8] 'haskell-navigate-imports)

;;     ;; Jump to the definition of the current symbol.
;;     (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

;;     ;; Indent the below lines on columns after the current column.
;;     (define-key haskell-mode-map (kbd "C-<right>")
;;       (lambda ()
;;         (interactive)
;;         (haskell-move-nested 1)))
;;     ;; Same as above but backwards.
;;     (define-key haskell-mode-map (kbd "C-<left>")
;;       (lambda ()
;;         (interactive)
;;         (haskell-move-nested -1))))

;; ;; Useful to have these keybindings for .cabal files, too.
;;   (defun haskell-cabal-hook ()
;;     (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;;     (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
;;     (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
;;     (define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch))

;; (eval-after-load 'haskell-mode
;;     '(progn
;;        (defun prelude-haskell-mode-defaults ()
;;          (subword-mode +1)
;;          (eldoc-mode +1)
;;          (haskell-indentation-mode +1)
;;          (interactive-haskell-mode +1)
;;          (haskell-hook))
;;        (setq prelude-haskell-mode-hook 'prelude-haskell-mode-defaults)

;;        (add-hook 'haskell-mode-hook (lambda ()
;;                                       (run-hooks 'prelude-haskel-mode-hook)))))
