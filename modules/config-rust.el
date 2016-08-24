;;; package --- Summary:
;; config-rust.el
;;
;;; Commentary:
;;
;;; Code:

(setq racer-cmd "/Users/srbutler/.cargo/bin/racer")
;; (setq racer-rust-src-path "/usr/local/share/doc/rust/html/src/")
(setq racer-rust-src-path "/usr/local/lib/rustlib/")

(use-package racer
  :ensure t
  :init (add-hook 'rust-mode-hook 'racer-activate)
  :config

  ;; Key binding to jump to method definition
  (local-set-key (kbd "M-.") #'racer-find-definition)

  (use-package flycheck-rust
    :ensure t
    :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    )

  (with-eval-after-load "company"
    (use-package company-racer
      :ensure t
      :init (add-to-list 'company-backends 'company-racer)))
  )

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :config

  ;; hooks
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  )

;;; config-rust.el ends here
 
