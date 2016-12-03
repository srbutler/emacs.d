;;; lang-lisp.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

;; COMMON LISP
(use-package lisp-mode
  :mode (("\\.cl\\'" . lisp-mode)
         ("\\.lisp\\'" . lisp-mode)
         ("\\.sbclrc\\'" . lisp-mode))
  :config
  (add-hook 'lisp-mode-hook 'smartparens-strict-mode)
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook 'eldoc-mode))

;; common-lisp REPL
(use-package slime
  :ensure t
  :commands (slime slime-mode)
  :init
  (progn
    (setq slime-contribs '(slime-fancy
                           slime-indentation
                           slime-sbcl-exts
                           slime-scratch))

    (defun slime/disable-smartparens ()
      (smartparens-strict-mode -1)
      (turn-off-smartparens-mode))
    (add-hook 'slime-repl-mode-hook #'slime/disable-smartparens))

  (setq slime-lisp-implementations
      '((ccl ("ccl"))
        (clisp ("clisp" "-q"))
        (cmucl ("cmucl" "-quiet"))
        (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))
  (setq slime-default-lisp 'ccl)

  :config
  (eval-after-load "slime"
  '(progn
     (setq slime-completion-at-point-functions 'slime-fuzzy-complete-symbol
           slime-complete-symbol*-fancy t
           slime-fuzzy-completion-in-place t
           slime-enable-evaluate-in-emacs t
           slime-autodoc-use-multiline-p t
           slime-auto-start 'always)

     (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
     (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector))))


;; EMACS LISP
;; keep .elc files updated automatically
(defun recompile-elc-on-save ()
  "Recompile your elc when saving an elisp file."
  (add-hook 'after-save-hook
            (lambda ()
              (when (and
                     (or (string-prefix-p modules-dir (file-truename buffer-file-name))
                         (string-prefix-p dotfiles-dir (file-truename buffer-file-name)))
                     (file-exists-p (byte-compile-dest-file buffer-file-name)))
                (emacs-lisp-byte-compile)))
            nil
            t))

(defun srb-emacs-lisp-mode-defaults ()
  "Sensible defaults for `emacs-lisp-mode'."
  (eldoc-mode +1)
  (paredit-mode +1)
  (rainbow-mode +1)
  (rainbow-delimiters-mode +1)
  (smartparens-strict-mode +1)
  (recompile-elc-on-save)
  (setq mode-name "Elisp"))

(setq srb-emacs-lisp-mode-hook 'srb-emacs-lisp-mode-defaults)
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (run-hooks 'srb-emacs-lisp-mode-hook)))

;; use emacs lisp mode to edit cask files
(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))


;; SCHEME/RACKET
;; make racket files open as scheme
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))

;; REPL and basic scheme hooks
(use-package geiser
  :ensure t
  :config
  ;; run geiser whenever a scheme file is opened
  (add-hook 'scheme-mode-hook 'geiser-mode)

  ;; regular lisp defaults
  (add-hook 'geiser-mode-hook 'smartparens-strict-mode)
  (add-hook 'geiser-mode-hook 'paredit-mode)
  (add-hook 'geiser-mode-hook 'rainbow-delimiters-mode)

  ;; turn off eldoc for geiser
  (add-hook 'geiser-mode-hook (lambda () (eldoc-mode nil)))
  
  ;; default to racket
  (setq geiser-default-implementation 'racket)

  ;; hooks for geiser-REPL
  (add-hook 'geiser-repl-mode-hook 'smartparens-mode)
  (add-hook 'geiser-repl-mode-hook 'rainbow-delimiters-mode))

(provide 'lang-lisp)
;;; lang-lisp.el ends here
