;;; lang-c.el --- Summary:
;;
;;; Commentary:
;;
;; Most configuration details borrowed from:
;; http://syamajala.github.io/c-ide.html
;; http://tuhdo.github.io/c-ide.html
;;
;;; Code:


(use-package cc-mode
  :defer t
  :config
  (font-lock-add-keywords
   'c-mode '(("\\<[\\+-]?[0-9]+\\(.[0-9]+\\)?\\>" 0 'font-lock-constant-face)))
  (font-lock-add-keywords
   'c++-mode '(("\\<[\\+-]?[0-9]+\\(.[0-9]+\\)?\\>" 0 'font-lock-constant-face)))

  ;; via https://stackoverflow.com/questions/30949847/configuring-flycheck-to-work-with-c11
  ;; this defaults the standard to c++11, should use dir local variables in most cases
  (add-hook 'c++-mode-hook
            (lambda () (progn
                         (setq flycheck-gcc-language-standard "c++11")
                         (setq flycheck-clang-language-standard "c++11")))))


;; use with lsp-mode
(use-package cquery
  :when (executable-find "cquery")
  :after lsp-mode
  :hook ((c-mode . lsp-cquery-enable)
         (c++-mode . lsp-cquery-enable))
  :custom (lsp-response-timeout 45)
  :config (require 'lsp-flycheck))


(use-package google-c-style
  :ensure
  :defer t
  :hook ((c-common-mode . google-set-c-style)
         (c-common-mode . google-make-newline-indent)))


(use-package clang-format
  :ensure t
  :after cc-mode
  :bind (:map c-mode-base-map ("C-c C-f" . clang-format-buffer))
  :config (setq clang-format-style "llvm"))


(use-package irony
  :ensure t
  :defer t
  :diminish "irony"
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  :config
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async)

  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))


(use-package company-irony
  :ensure t
  :defer t
  :init
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (eval-after-load 'company
    '(progn
       (delete 'company-semantic company-backends)
       (add-to-list 'company-backends 'company-irony)))
  :custom (company-idle-delay 0))


(use-package company-irony-c-headers
  :ensure t
  :defer t
  :init
  (eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony-c-headers)))


(use-package irony-eldoc
  :ensure t
  :defer t
  :init (add-hook 'irony-mode-hook 'irony-eldoc))


(use-package cmake-ide
  :ensure t
  :defer t
  :init (add-hook 'c-mode-common-hook 'cmake-ide-setup))


(provide 'lang-c)
;;; lang-c.el ends here
