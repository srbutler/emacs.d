;;; lang-rust.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)

  ;; (setq mode-name "Rust ")
  )

(use-package racer
  :ensure t
  :config
  (progn
    (setq company-tooltip-align-annotations t
          racer-cmd "/Users/srbutler/.cargo/bin/racer"
          racer-rust-src-path "/Users/srbutler/.multirust/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src")
    (add-hook 'rust-mode-hook  #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode))
  
  ;; Key binding to jump to method definition
  (local-set-key (kbd "M-.") #'racer-find-definition))

(use-package company-racer
  :ensure t
  :init
  (with-eval-after-load "company"
    (add-to-list 'company-backends 'company-racer)))

(use-package flycheck-rust
  :ensure t
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package cargo
  :ensure t
  :diminish (cargo-minor-mode . "cargo")
  :init (add-hook 'rust-mode-hook 'cargo-minor-mode))

(provide 'lang-rust)
;;; lang-rust.el ends here
