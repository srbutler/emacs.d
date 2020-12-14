;;; lang-cc.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(defalias 'cpp-mode 'c++-mode)
(defvaralias 'cpp-mode-map 'c++-mode-map)

(use-package cc-mode
  :mode ("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
  :config
  (font-lock-add-keywords
   'c-mode '(("\\<[A-Z]*_[0-9A-Z_]+\\>" 0 'font-lock-constant-face)
             ("\\<[A-Z]\\{3,\\}\\>"  0 'font-lock-constant-face)))
  (font-lock-add-keywords
   'c++-mode '(("\\<[A-Z]*_[0-9A-Z_]+\\>" 0 'font-lock-constant-face)
             ("\\<[A-Z]\\{3,\\}\\>"  0 'font-lock-constant-face))))


;; slightly better font-lock for c++
(use-package modern-cpp-font-lock
  :ensure t
  :hook ((c++-mode . modern-c++-font-lock-mode)))


;; lsp server
(use-package ccls
  :ensure t
  :when (executable-find "ccls")
  :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls) (lsp)))
  :init
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
    (add-to-list 'projectile-project-root-files-bottom-up ".ccls-root")
    (add-to-list 'projectile-project-root-files-top-down-recurring "compile_commands.json"))
  :config
  (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t))))


(use-package flycheck-clang-analyzer
  :ensure t
  :after flycheck
  :config (flycheck-clang-analyzer-setup))


(use-package google-c-style
  :ensure t
  :defer t
  :hook ((c-common-mode-hook . google-set-c-style)
         (c-common-mode-hook . google-make-newline-indent)))


(use-package clang-format
  :ensure t
  :after cc-mode
  :bind (:map c-mode-base-map ("C-c C-f" . clang-format-buffer))
  :config (setq clang-format-style "llvm"))


(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists.txt" "\\.cmake\\'"))


(use-package cmake-font-lock
  :ensure t
  :hook (cmake-mode . cmake-font-lock-activate))


(provide 'lang-cc)
;;; lang-cc.el ends here
