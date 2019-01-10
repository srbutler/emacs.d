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
         ("\\.sbclrc\\'" . lisp-mode)))


;; SLIME replacement
(use-package sly
  :ensure t
  :bind (:map lisp-mode-map ("C-c C-z" . sly))
  :hook ((sly-mrepl-mode . rainbow-delimiters-mode)
         (sly-mrepl-mode . smartparens-strict-mode))
  :config
  (setq sly-lisp-implementations
        '((sbcl ("sbcl"  "--noinform") :coding-system utf-8-unix)
          (ccl ("ccl64")))))


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
  :hook ((slime-repl-mode . rainbow-delimiters-mode)
         (slime-repl-mode . smartparens-strict-mode))
  :config
  (slime-setup '(slime-company
                 slime-fancy
                 slime-indentation
                 slime-sbcl-exts
                 slime-scratch))

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
  :commands geiser-default-implementation
  :hook ((scheme-mode . geiser-mode)
         (geiser-repl-mode . rainbow-delimiters-mode)
         (geiser-repl-mode . smartparens-strict-mode))
  :config (setq geiser-default-implementation 'racket))


(provide 'lang-lisp)
;;; lang-lisp.el ends here
