;;; lang-rust.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :bind (:map rust-mode-map ("C-c C-f" . rust-format-buffer))
  :config
  ;; disable matching for single quotes
  (sp-local-pair 'rust-mode "'" nil :actions nil))


;; set up LSP server for Rust
(use-package lsp-rust
  :ensure t
  :hook ((rust-mode . lsp-rust-enable)))


(use-package racer
  :disabled t
  :ensure t
  :after rust-mode
  :bind (:map rust-mode-map
              ("M-." . racer-find-definition)
              ("M-," . pop-tag-mark)
              ("TAB" . company-indent-or-complete-common))
  :init (add-hook 'rust-mode-hook 'racer-mode)
  :custom
  (company-tooltip-align-annotations t)
  (racer-cmd "~/.cargo/bin/racer")
  (racer-rust-src-path "~/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src")
  :config
  (add-hook 'racer-mode-hook 'eldoc-mode))

(use-package flycheck-rust
  :disabled t
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
