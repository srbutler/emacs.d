;;; lang-cc.el --- Summary:
;;
;;; Commentary:
;;
;; Many configuration details borrowed from:
;; http://syamajala.github.io/c-ide.html
;; http://tuhdo.github.io/c-ide.html
;;
;;; Code:

;; switch use of LSP versus Irony easily
(defvar *cc-use-lsp* t)

(use-package cc-mode
  :mode ("\\.h\\'" . c++-mode)  ;; headers default to C++
  :config
  (font-lock-add-keywords
   'c-mode '(("\\<[\\+-]?[0-9]+\\(.[0-9]+\\)?\\>" 0 'font-lock-constant-face)))
  (font-lock-add-keywords
   'c++-mode '(("\\<[\\+-]?[0-9]+\\(.[0-9]+\\)?\\>" 0 'font-lock-constant-face)))

  ;; via https://stackoverflow.com/questions/30949847/configuring-flycheck-to-work-with-c11
  ;; this defaults the standard to c++14, should use dir local variables in most cases
  (with-eval-after-load 'flycheck
    (setq flycheck-gcc-language-standard "c++14")
    (setq flycheck-clang-language-standard "c++14")))


;; slightly better font-lock for c++
(use-package modern-cpp-font-lock
  :ensure t
  :defer t
  :hook ((c++-mode . modern-c++-font-lock-mode)))


;; lsp server
;; install with `brew tap twlz0ne/homebrew-ccls && brew install ccls'
(use-package ccls
  :when *cc-use-lsp*
  :ensure t
  :hook ((c-mode   . lsp)
         (c++-mode . lsp))
  :commands projectile-project-root-files-top-down-recurring
  :config
  (let ((ccls (executable-find "ccls")))
    (if ccls
        (setq ccls-executable ccls)
      (message "Could not locate `ccls' executable!")))

  ;; adds irony-style detailed labels
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


;; install: pip3 install cstyle
(use-package flycheck-cstyle
  :disabled t
  :ensure t
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
  :unless *cc-use-lsp*
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
  :unless *cc-use-lsp*
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
  :unless *cc-use-lsp*
  :disabled t
  :ensure t
  :defer t
  :init
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony-c-headers)))


(use-package irony-eldoc
  :unless *cc-use-lsp*
  :disabled t
  :ensure t
  :defer t
  :init (add-hook 'irony-mode-hook 'irony-eldoc))


(use-package cmake-ide
  :disabled t
  :ensure t
  :defer t
  :init (add-hook 'c-mode-common-hook 'cmake-ide-setup))


(provide 'lang-cc)
;;; lang-cc.el ends here
