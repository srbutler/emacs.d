;;; lang-rust.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(defvar *rust-use-lsp* nil)

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :bind (:map rust-mode-map ("C-c C-f" . rust-format-buffer))
  :config
  ;; disable matching for single quotes
  (sp-local-pair 'rust-mode "'" nil :actions nil))

(use-package racer
  :unless *rust-use-lsp*
  :ensure t
  :bind (:map rust-mode-map
              ("M-." . racer-find-definition)
              ("M-," . pop-tag-mark)
              ("TAB" . company-indent-or-complete-common))
  :hook (rust-mode . racer-mode)
  :custom
  (company-tooltip-align-annotations t)
  (racer-cmd "~/.cargo/bin/racer")
  (racer-rust-src-path "~/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src")
  :config
  (add-hook 'racer-mode-hook 'eldoc-mode))


;; set up LSP server for Rust
;; install: rustup component add rls-preview rust-analysis rust-src
(use-package lsp-rust
  :if *rust-use-lsp*
  :ensure t
  :hook ((rust-mode . lsp-rust-enable)))


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
