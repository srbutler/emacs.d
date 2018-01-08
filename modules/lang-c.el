;;; lang-c.el --- Summary:
;;
;;; Commentary:
;;
;; Most configuration details borrowed from:
;; http://syamajala.github.io/c-ide.html
;; http://tuhdo.github.io/c-ide.html
;;
;;; Code:

;; highlight numbers correctly
(font-lock-add-keywords
 'cc-mode '(("\\<[\\+-]?[0-9]+\\(.[0-9]+\\)?\\>" 0 'font-lock-constant-face)))

;; load Google's C/C++ style file
(load
 (expand-file-name "google-c-style.el"
                   (expand-file-name "google_c_style" vendor-dir)))

(add-hook 'c-common-mode-hook 'google-set-c-style)
(add-hook 'c-common-mode-hook 'google-make-newline-indent)


(use-package clang-format
  :ensure t
  :demand
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
  :config
  (setq company-idle-delay 0)
  ;; (define-key c-mode-map [(tab)] 'company-complete)
  ;; (define-key c++-mode-map [(tab)] 'company-complete)
  )


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
