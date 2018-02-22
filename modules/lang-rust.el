;;; lang-rust.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :bind (:map rust-mode-map ("C-c C-f" . rust-format-buffer)))

(use-package racer
  :ensure t
  :defer t
  :bind (:map rust-mode-map ("M-." . racer-find-definition))
  :config
  (progn
    (setq company-tooltip-align-annotations t
          racer-cmd "~/.cargo/bin/racer"
          racer-rust-src-path "~/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src")
    (add-hook 'rust-mode-hook  'racer-mode)
    (add-hook 'racer-mode-hook 'eldoc-mode)
    (add-hook 'racer-mode-hook 'company-mode)))

(use-package company-racer
  :ensure t
  :defer t
  :init
  (with-eval-after-load "company"
    (add-to-list 'company-backends 'company-racer)))

(use-package flycheck-rust
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))

(use-package cargo
  :ensure t
  :defer t
  :diminish (cargo-minor-mode . "cargo")
  :init (add-hook 'rust-mode-hook 'cargo-minor-mode))

(provide 'lang-rust)
;;; lang-rust.el ends here
