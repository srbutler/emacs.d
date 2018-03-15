;;; lang-lisp.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

;; EMACS LISP
;; smartly visit an ielm buffer
(defun visit-ielm ()
  "Create or visit a `ielm' buffer."
  (interactive)
  (if (not (get-buffer "*ielm*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ielm))
    (switch-to-buffer-other-window "*ielm*")))


(use-package emacs-lisp-mode
  :ensure nil
  :mode (("\\.el\\'" . emacs-lisp-mode)
         ("Cask\\'" . emacs-lisp-mode))
  :bind (:map emacs-lisp-mode-map ("C-c C-z" . visit-ielm))
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
  (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  :config
  (setq mode-name "ELisp")
  ;; keep .elc files updated automatically
  (add-hook 'after-save-hook
            (lambda ()
              (when (and
                     (or (string-prefix-p *modules-dir* (file-truename buffer-file-name))
                         (string-prefix-p *dotfiles-dir* (file-truename buffer-file-name)))
                     (file-exists-p (byte-compile-dest-file buffer-file-name)))
                (emacs-lisp-byte-compile)))
            nil
            t))

(use-package ielm
  :ensure nil
  :config
  (add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook 'smartparens-strict-mode)
  (add-hook 'ielm-mode-hook 'paredit-mode))


;; COMMON LISP
(use-package lisp-mode
  :ensure nil
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
  :defer t
  :bind (:map slime-mode-map ("C-c C-s" . slime-selector))
  :commands (slime slime-mode)
  :config
  (setq tab-always-indent 'complete)
  (setq slime-contribs '(slime-fancy
                         slime-indentation
                         slime-sbcl-exts
                         slime-scratch))

  (defun slime/disable-smartparens ()
    (smartparens-strict-mode -1)
    (turn-off-smartparens-mode))
  (add-hook 'slime-repl-mode-hook #'slime/disable-smartparens)

  (setq slime-lisp-implementations
        '((ccl ("ccl"))
          (clisp ("clisp" "-q"))
          (cmucl ("cmucl" "-quiet"))
          (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))
  (setq slime-default-lisp 'sbcl)

  (eval-after-load "slime"
    '(progn
       (setq slime-completion-at-point-functions 'slime-fuzzy-complete-symbol
             slime-complete-symbol*-fancy t
             slime-fuzzy-completion-in-place t
             slime-enable-evaluate-in-emacs t
             slime-autodoc-use-multiline-p t
             slime-auto-start 'always))))


;; SCHEME/RACKET
(use-package scheme
  :defer t
  :mode (("\\.scm\\'" . scheme-mode)
         ("\\.rkt\\'" . scheme-mode)))


;; REPL and basic scheme hooks
(use-package geiser
  :ensure t
  :defer t
  :init (add-hook 'scheme-mode-hook 'geiser-mode)
  :config

  ;; regular lisp defaults
  (add-hook 'geiser-mode-hook 'smartparens-strict-mode)
  (add-hook 'geiser-mode-hook 'paredit-mode)
  (add-hook 'geiser-mode-hook 'rainbow-delimiters-mode)

  ;; default to racket
  (setq geiser-default-implementation 'racket)

  ;; hooks for geiser-REPL
  (add-hook 'geiser-repl-mode-hook 'smartparens-mode)
  (add-hook 'geiser-repl-mode-hook 'rainbow-delimiters-mode))

(provide 'lang-lisp)
;;; lang-lisp.el ends here
