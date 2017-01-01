;;; lang-c.el --- Summary:
;;
;;; Commentary:
;;
;; Most configuration details borrowed from:
;; http://syamajala.github.io/c-ide.html
;; http://tuhdo.github.io/c-ide.html
;;
;;; Code:

;; (defun srb-c-mode-common-hooks ()
;;   (setq
;;         c-default-style "java"
;;         c-basic-offset 4
;;         tab-width 4
;;         tab-always-indent 'complete
;;         ))
;; (add-hook 'c-mode-common-hook #'srb-c-mode-common-hooks)

(font-lock-add-keywords 'cc-mode
                          '(("\\<[\\+-]?[0-9]+\\(.[0-9]+\\)?\\>" 0 'font-lock-constant-face)))

;; load Google's C/C++ style file
(load
 (expand-file-name "google-c-style.el"
                   (expand-file-name "google_c_style" vendor-dir)))

(add-hook 'c-common-mode-hook 'google-set-c-style)
(add-hook 'c-common-mode-hook 'google-make-newline-indent)

(use-package clang-format
  :ensure t
  :bind ("C-c C-f" . clang-format-region)
  :config
  (setq clang-format-style "google"))

(use-package irony
  :ensure t
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
  :config
  (eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony))))


(use-package irony-eldoc
  :ensure t
  :init (add-hook 'irony-mode-hook 'irony-eldoc))


(use-package cmake-ide
  :ensure t
  :init (add-hook 'c-mode-common-hook 'cmake-ide-setup))


(use-package rtags
  :disabled t
  :ensure t
  :init
  (eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))

  (defun my-flycheck-rtags-setup ()
    (flycheck-select-checker 'rtags)
    (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
    (setq-local flycheck-check-syntax-automatically nil))
  ;; c-mode-common-hook is also called by c++-mode
  (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)

  :config
  (setq rtags-autostart-diagnostics t
        rtags-completions-enabled t
        rtags-use-helm t)

  (rtags-enable-standard-keybindings))

(provide 'lang-c)
;;; lang-c.el ends here
