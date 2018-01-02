;;; lang-ocaml.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

;; Ocaml major mode
(use-package tuareg
  :ensure t
  :mode (("\\.ml[ily]?$"  . tuareg-mode)
         ("\\.topml$"     . tuareg-mode)
         ("\\.ocamlinit$" . tuareg-mode))
  :init
  ;; Make OCaml-generated files invisible to filename completion
  (dolist
      (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi" ".cmxs" ".cmt" ".cmti" ".annot"))
    (add-to-list 'completion-ignored-extensions ext))
  :config 
  ;; disable backtick pairing
  (sp-pair "`" nil :actions :rem))


;; completion engine
(use-package merlin
  :ensure t
  :defer t
  :init
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'merlin-company-backend)))


;; error checking
(use-package flycheck-ocaml
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'merlin
    ;; Disable Merlin's own error checking
    (setq merlin-error-after-save nil)

    ;; Enable Flycheck checker
    (flycheck-ocaml-setup)))


;; REPL control
(use-package utop
  :ensure t
  :defer t
  :init
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  (add-hook 'reason-mode-hook 'utop-minor-mode)
  :config
  (when (executable-find "opam")
      (setq utop-command "opam config exec -- utop -emacs")))


;; indenting/formatting
(use-package ocp-indent
  :ensure t
  :defer t
  :init (add-hook 'tuareg-mode-hook 'ocp-indent-caml-mode-setup))


;; taken from spacemacs (package not in [M]ELPA)
(use-package merlin-eldoc
  :defer t
  :load-path "~/.emacs.d/vendor/merlin-eldoc/merlin-eldoc.el"
  :init (add-hook 'merlin-mode-hook #'merlin-eldoc/setup))


;; for using Reason's syntax instead of OCaml's
(use-package reason-mode
  :ensure t
  :mode ("\\.rei?$"  . reason-mode))


(provide 'lang-ocaml)
;;; lang-ocaml.el ends here

