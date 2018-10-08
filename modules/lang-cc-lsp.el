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
  :mode ("\\.h\\'" . c++-mode)  ;; headers default to C++
  :config
  (font-lock-add-keywords
   'c-mode '(("\\<[\\+-]?[0-9]+\\(.[0-9]+\\)?\\>" 0 'font-lock-constant-face)))
  (font-lock-add-keywords
   'c++-mode '(("\\<[\\+-]?[0-9]+\\(.[0-9]+\\)?\\>" 0 'font-lock-constant-face)))

  ;; via https://stackoverflow.com/questions/30949847/configuring-flycheck-to-work-with-c11
  ;; this defaults the standard to c++11, should use dir local variables in most cases
  (with-eval-after-load 'flycheck
    (setq flycheck-gcc-language-standard "c++14")
    (setq flycheck-clang-language-standard "c++14")))


;; slightly better font-lock for c++
(use-package modern-cpp-font-lock
  :ensure t
  :defer t
  :hook ((c++-mode . modern-c++-font-lock-mode)))


;; lsp server
(use-package ccls
  :ensure t
  :ensure-system-package (ccls . "brew tap twlz0ne/homebrew-ccls && brew install ccls")
  :hook ((c-mode   . lsp-ccls-enable)
         (c++-mode . lsp-ccls-enable))
  :commands projectile-project-root-files-top-down-recurring
  :config
  (setq ccls-executable "/usr/local/bin/ccls")
  ;; adds irony-style detailed laels
  (setq ccls-extra-init-params '(:completion (:detailedLabel t)))

  (with-eval-after-load 'projectile
    (setq projectile-project-root-files-top-down-recurring
          (append '("compile_commands.json"
                    ".ccls")
                  projectile-project-root-files-top-down-recurring))))


(use-package flycheck-clang-analyzer
  :ensure t
  :after flycheck
  :commands flycheck-clang-analyzer-executable
  :config
  (progn
    (setq flycheck-clang-analyzer-executable "clang")
    (flycheck-clang-analyzer-setup)
    ;; automatically sets itself up as next checker after lsp-ui so undo
    ;; that so is instead after cppcheck
    (delete '(warning . clang-analyzer)
            (flycheck-checker-get 'lsp-ui 'next-checkers))
    (flycheck-add-next-checker 'c/c++-cppcheck '(t . clang-analyzer))))


(use-package flycheck-cstyle
  :disabled t
  :ensure t
  :ensure-system-package (cstyle . "pip3 install cstyle")
  :after lsp-ui
  :config
  (progn
    (flycheck-cstyle-setup)
    (flycheck-add-next-checker 'lsp-ui '(warning . cstyle))
    (flycheck-add-next-checker 'cstyle '(t . c/c++-cppcheck))))


(use-package google-c-style
  :ensure t
  :defer t
  :init
  (add-hook 'c-common-mode-hook 'google-set-c-style)
  (add-hook 'c-common-mode-hook 'google-make-newline-indent))


(use-package clang-format
  :ensure t
  :after cc-mode
  :bind (:map c-mode-base-map ("C-c C-f" . clang-format-buffer))
  :config (setq clang-format-style "llvm"))


(use-package irony
  :disabled t
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
  :disabled t
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
  :disabled t
  :ensure t
  :defer t
  :init
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony-c-headers)))


(use-package irony-eldoc
  :disabled t
  :ensure t
  :defer t
  :init (add-hook 'irony-mode-hook 'irony-eldoc))


(use-package cmake-ide
  :disabled t
  :ensure t
  :defer t
  :init (add-hook 'c-mode-common-hook 'cmake-ide-setup))


(provide 'lang-c)
;;; lang-c.el ends here
