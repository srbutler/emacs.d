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
   'cc-mode '(("\\<[\\+-]?[0-9]+\\(.[0-9]+\\)?\\>" 0 'font-lock-constant-face)))

  ;; via https://stackoverflow.com/questions/30949847/configuring-flycheck-to-work-with-c11
  ;; this defaults the standard to c++11, should use dir local variables in most cases
  (add-hook 'c++-mode-hook
            (lambda () (progn
                         (setq flycheck-gcc-language-standard "c++11")
                         (setq flycheck-clang-language-standard "c++11")))))


;; better highlighting for C++14 onward
(use-package modern-cpp-font-lock
  :ensure t
  :defer t
  :diminish modern-c++-font-lock-mode
  :init (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))


(use-package google-c-style
  :ensure
  :defer t
  :init
  (add-hook 'c-common-mode-hook 'google-set-c-style)
  (add-hook 'c-common-mode-hook 'google-make-newline-indent))


(use-package clang-format
  :ensure t
  :demand
  :config
  (setq clang-format-style "llvm")

  ;; bind it as a hook instead, :bind is not working correctly
  (add-hook 'c-mode-common-hook
            (lambda ()
              (bind-key "C-c C-f" #'clang-format-buffer c-mode-base-map))))


(use-package irony
  :ensure t
  :defer t
  :diminish "irony"
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)

  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))

  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))


(use-package company-irony
  :ensure t
  :defer t
  :init
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (setq company-backends
        (delete 'company-semantic company-backends))
  ;; (eval-after-load 'company
  ;;   '(add-to-list 'company-backends 'company-irony))
  :config (setq company-idle-delay 0))


(use-package company-irony-c-headers
  :ensure t
  :defer t
  :config
  (eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony))))


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
