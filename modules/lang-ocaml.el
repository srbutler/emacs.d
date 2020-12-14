;;; lang-ocaml.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

;; install: opam install ocaml-lsp-server

;; Ocaml major mode
(use-package tuareg
  :ensure t
  :mode (("\\.ml[ily]?$"  . tuareg-mode)
         ("\\.topml$"     . tuareg-mode)
         ("\\.ocamlinit$" . tuareg-mode))
  :hook (tuareg-mode . lsp)
  :init
  ;; Make OCaml-generated files invisible to filename completion
  (dolist
      (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi" ".cmxs" ".cmt" ".cmti" ".annot"))
    (add-to-list 'completion-ignored-extensions ext)))


(use-package dune
  :ensure t)


;; REPL control
(use-package utop
  :ensure t
  :defer t
  :config
  (when (executable-find "opam")
    (setq utop-command "opam config exec -- utop -emacs")))


;; indenting/formatting
(use-package ocp-indent
  :ensure t
  :bind (:map tuareg-mode-map ("C-c C-f" . ocp-indent-buffer))
  :hook (tuareg-mode . ocp-setup-indent))


(provide 'lang-ocaml)
;;; lang-ocaml.el ends here
