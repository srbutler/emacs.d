;;; lang-rust.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(use-package rust-mode
  :ensure t
  :ensure-system-package (rustup . "curl https://sh.rustup.rs -sSf | sh")
  :mode ("\\.rs\\'" . rust-mode)
  :bind (:map rust-mode-map ("C-c C-f" . rust-format-buffer))
  :config
  ;; disable matching for single quotes
  (sp-local-pair 'rust-mode "'" nil :actions nil))


(use-package cargo
  :ensure t
  :ensure-system-package
  ((rustfmt     . "rustup component add rustfmt-preview")
   (clippy      . "cargo install clippy")
   (cargo-check . "cargo install cargo-check")
   (cargo-edit  . "cargo install cargo-edit"))
  :defer t
  :diminish (cargo-minor-mode . "cargo")
  :init (add-hook 'rust-mode-hook 'cargo-minor-mode))


;; set up LSP server for Rust
(use-package lsp-rust
  :ensure t
  :ensure-system-package
  (rls . "rustup component add rls-preview rust-analysis rust-src")
  :hook ((rust-mode . lsp-rust-enable)))


(use-package racer
  :disabled t
  :ensure t
  :ensure-system-package (racer . "cargo +nightly install racer")
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



(provide 'lang-rust)
;;; lang-rust.el ends here
