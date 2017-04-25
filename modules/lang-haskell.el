;;; lang-haskell.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:


(use-package haskell-mode
  :defer t
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.cabal$". haskell-mode))
  :init
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
  (add-hook 'haskell-mode-hook 'subword-mode)
  (setq haskell-process-type 'stack-ghci
        ;; haskell-process-args-stack-ghci '("--ghc-options=-ferror-spans" "--with-ghc=intero")
        haskell-process-suggest-add-package nil
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-ask-also-kill-buffers nil
        haskell-stylish-on-save t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t
        haskell-interactive-popup-errors nil)
  :config
  (define-key haskell-mode-map (kbd "M-,") #'pop-tag-mark)
  (define-key haskell-mode-map (kbd "C-,") #'haskell-move-nested-left)
  (define-key haskell-mode-map (kbd "C-.") #'haskell-move-nested-right)
  (define-key haskell-mode-map (kbd "C-c C-c") #'haskell-compile)
  (define-key haskell-mode-map (kbd "C-c C-z") #'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c t") #'haskell-mode-show-type-at)
  (define-key haskell-mode-map (kbd "C-c v c") #'haskell-cabal-visit-file)
  (define-key haskell-mode-map (kbd "C-c <") #'haskell-move-nested-left)
  (define-key haskell-mode-map (kbd "C-c >") #'haskell-move-nested-right)
  (define-key haskell-mode-map (kbd "M-RET")
    (defun haskell-mode-open-line (n-todo)
      (interactive "P")
      (insert "\n")
      (backward-char)))

  (defun haskell-do-info (&optional cPos cEnd)
    "Bring up REPL and do :info on symbol at point.
If interactive and region active or CPOS and CEND are non-nil, use that region."
    (interactive "r")
    (let ((symbol (if (and (and cPos cEnd)
                           (or (region-active-p)
                               (not (called-interactively-p 'interactive))))
                      (buffer-substring-no-properties cPos cEnd)
                    (thing-at-point 'symbol))))
      (haskell-interactive-switch)
      (haskell-interactive-mode-run-expr (format ":info %s" symbol)))
    (goto-char (point-max))
    (haskell-interactive-switch-back)))


(use-package intero
  :ensure t
  :defer t
  :diminish (intero-mode . "intero")
  :init
  (add-hook 'haskell-mode-hook #'intero-mode))


(use-package haskell-hoogle
  :defer t
  :init
  (defun let-browse-url-chrome (orig-fun &rest args)
    (let ((browse-url-browser-function #'browse-url-chrome))
      (apply orig-fun args)))
  (advice-add #'hoogle :around #'let-browse-url-chrome)
  (defun hoogle-chrome ()
    (interactive)
    (let (haskell-hoogle-command)
      (call-interactively #'hoogle))))


(use-package haskell-doc
  :defer t
  :init
  (defun haskell-doc-show-type-unless-showing (orig-fun &rest args)
    (when (not (memq last-command '(haskell-mode-show-type-at
                                    intero-type-at
                                    haskell-process-do-type)))
      (apply orig-fun args)))
  (advice-add #'haskell-doc-mode-print-current-symbol-info
              :around #'haskell-doc-show-type-unless-showing))


(use-package hindent
  :defer t
  :init
  (when (locate-library "hindent")
    (add-hook 'haskell-mode-hook #'hindent-mode)))


(use-package company-ghci
  :ensure t
  :after haskell
  :config
  (push
   '(company-ghci :with company-yasnippet :with company-dabbrev)
   company-backends))


;; Smartparens:
(add-hook 'smartparens-mode-hook
          (lambda ()
            (add-to-list 'sp-no-reindent-after-kill-modes #'haskell-mode)
            (require 'smartparens-haskell) ; should add {-# #-}
            (sp-local-pair 'haskell-mode "'" nil :actions nil)
            (sp-local-pair 'haskell-mode "\\(" nil :actions nil)
            (sp-local-pair 'interactive-haskell-mode "\\(" nil :actions nil)
            (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)))


;; (use-package haskell-mode
;;   :ensure t
;;   :mode (("\\.hs\\'" . haskell-mode)
;;          ("\\.cabal$". haskell-mode))
;;   :init
;;   (add-hook 'haskell-mode-hook 'subword-mode)
;;   (add-hook 'haskell-mode-hook 'haskell-doc-mode)
;;   (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;;   (add-hook 'haskell-mode-hook 'haskell-indentation-mode)

;;   :config
;;   (setq 
;;         haskell-compile-cabal-build-command "stack build"
;;         haskell-font-lock-symbols nil
;;         ;; haskell-interactive-mode-scroll-to-bottom t
;;         ;; haskell-interactive-popup-errors nil
;;         haskell-process-suggest-remove-import-lines t
;;         haskell-process-auto-import-loaded-modules t
;;         ;; haskell-process-load-or-reload-prompt t
;;         ;; haskell-process-log t
;;         ;; haskell-process-suggest-remove-import-lines  t
;;         haskell-process-type 'stack-ghci
;;         ;; haskell-process-type 'auto
;;         haskell-stylish-on-save t
;;         ;; haskell-completions-complete-operators t
;;         ;; haskell-notify-p t
;;         ;; haskell-doc-show-reserved nil
;;         ;; haskell-indent-spaces 4
;;         ;; haskell-indent-offset 4
;;         ;; haskell-doc-show-global-types t
;;       )
;;   )

(provide 'lang-haskell)
;;; lang-haskell.el ends here
