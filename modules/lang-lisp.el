;;; lang-lisp.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

;; common lisp
(use-package lisp-mode
  :ensure nil
  :mode (("\\.cl\\'" . lisp-mode)
         ("\\.lisp\\'" . lisp-mode)
         ("\\.sbclrc\\'" . lisp-mode))
  :config
  (add-hook 'lisp-mode-hook 'smartparens-strict-mode)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook 'eldoc-mode))


;; SLIME replacement
(use-package sly
  :ensure t
  :bind (:map lisp-mode-map ("C-c C-z" . sly))
  :config
  (setq inferior-lisp-program "sbcl")
  (add-hook 'sly-mrepl-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'sly-mrepl-mode-hook 'smartparens-strict-mode))


;; adds quickload command to sly
(use-package sly-quicklisp
  :ensure t)


;; adds macro expansion to sly
(use-package sly-macrostep
  :ensure t)


(use-package slime-company
  :disabled t
  :ensure t)


;; common-lisp REPL
(use-package slime
  :disabled t
  :ensure t
  :bind (:map slime-mode-map ("C-c C-s" . slime-selector))
  :config
  (slime-setup '(slime-company
                 slime-fancy
                 slime-indentation
                 slime-sbcl-exts
                 slime-scratch))

  (add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'slime-repl-mode-hook 'smartparens-strict-mode)

  (setq slime-lisp-implementations
        '((ccl ("ccl"))
          (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))

  (setq inferior-lisp-program "sbcl"
        slime-default-lisp 'sbcl
        slime-completion-at-point-functions 'slime-fuzzy-complete-symbol
        slime-complete-symbol*-fancy t
        slime-fuzzy-completion-in-place t
        slime-enable-evaluate-in-emacs t
        slime-autodoc-use-multiline-p t
        slime-auto-start 'always))


;; scheme and racket
(use-package scheme
  :defer t
  :mode (("\\.scm\\'" . scheme-mode)
         ("\\.rkt\\'" . scheme-mode)))


;; REPL and basic scheme hooks
(use-package geiser
  :ensure t
  :defer t
  :init (add-hook 'scheme-mode-hook 'geiser-mode)
  :commands geiser-default-implementation
  :config
  ;; default to racket
  (setq geiser-default-implementation 'racket)

  ;; regular lisp defaults
  (add-hook 'geiser-mode-hook 'smartparens-strict-mode)
  (add-hook 'geiser-mode-hook 'rainbow-delimiters-mode)

  ;; hooks for geiser-REPL
  (add-hook 'geiser-repl-mode-hook 'smartparens-mode)
  (add-hook 'geiser-repl-mode-hook 'rainbow-delimiters-mode))


(provide 'lang-lisp)
;;; lang-lisp.el ends here
