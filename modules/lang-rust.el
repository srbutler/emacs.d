;;; lang-rust.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(defvar *rust-use-lsp* t)
;; install: rustup component add rls-preview rust-analysis rust-src

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :bind (:map rust-mode-map ("C-c C-f" . rust-format-buffer))
  :init (when *rust-use-lsp* (add-hook 'rust-mode-hook 'lsp)))


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
  (racer-rust-src-path "~/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"))


(use-package flycheck-rust
  :ensure t
  :defer t
  :hook (flycheck-mode . flycheck-rust-setup))


(use-package cargo
  :ensure t
  :defer t
  :diminish (cargo-minor-mode . "cargo")
  :hook (rust-mode . cargo-minor-mode))


(provide 'lang-rust)
;;; lang-rust.el ends here
