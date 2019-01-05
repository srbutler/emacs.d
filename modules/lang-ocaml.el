;;; lang-ocaml.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(defvar *ocaml-use-lsp* nil)
;; install: npm i -g ocaml-language-server

;; Ocaml major mode
(use-package tuareg
  :ensure t
  :mode (("\\.ml[ily]?$"  . tuareg-mode)
         ("\\.topml$"     . tuareg-mode)
         ("\\.ocamlinit$" . tuareg-mode))
  :init
  (when *ocaml-use-lsp* (add-hook 'tuareg-mode-hook 'lsp))

  ;; Make OCaml-generated files invisible to filename completion
  (dolist
      (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi" ".cmxs" ".cmt" ".cmti" ".annot"))
    (add-to-list 'completion-ignored-extensions ext))

  ;; for dune build files (not in melpa currently)
  (use-package dune
    :if (file-exists-p "~/.opam/4.07.0/share/emacs/site-lisp/dune.el")
    :load-path "~/.opam/4.07.0/share/emacs/site-lisp/"))


;; completion engine
;; install: opam install merlin
(use-package merlin
  :unless *ocaml-use-lsp*
  :after company
  :ensure t
  :defer t
  :after tuareg company
  :defines company-backends
  :bind (:map tuareg-mode-map
              ("C-c C-t" . merlin-type-enclosing))
  :hook ((tuareg-mode reason-mode caml-mode) . merlin-mode)
  :init
  (setq merlin-completion-with-doc t)
  (add-to-list 'company-backends 'merlin-company-backend))


;; error checking
(use-package flycheck-ocaml
  :ensure t
  :defer t
  :after merlin
  :init
  ;; Disable Merlin's own error checking
  (setq merlin-error-after-save nil)

  ;; Enable Flycheck checker
  (flycheck-ocaml-setup))


;; REPL control
(use-package utop
  :ensure t
  :defer t
  :hook (tuareg-mode reason-mode)
  :config
  (when (executable-find "opam")
    (setq utop-command "opam config exec -- utop -emacs")))


;; indenting/formatting
(use-package ocp-indent
  :ensure t
  :bind (:map tuareg-mode-map ("C-c C-f" . ocp-indent-buffer))
  :hook (tuareg-mode . ocp-setup-indent))


(use-package merlin-eldoc
  :after merlin
  :ensure t
  :custom
  (eldoc-echo-area-use-multiline-p t) ; use multiple lines when necessary
  (merlin-eldoc-max-lines 8)          ; but not more than 8
  ;; (merlin-eldoc-type-verbosity 'min)  ; don't display verbose types
  ;; (merlin-eldoc-function-arguments nil) ; don't show function arguments
  ;; (merlin-eldoc-doc nil)                ; don't show the documentation
  :bind (:map merlin-mode-map
              ("C-c m p" . merlin-eldoc-jump-to-prev-occurrence)
              ("C-c m n" . merlin-eldoc-jump-to-next-occurrence))
  :hook ((tuareg-mode reason-mode) . merlin-eldoc-setup))


;; for using Reason's syntax instead of OCaml's
(use-package reason-mode
  :ensure t
  :mode ("\\.rei?$" . reason-mode)
  :init (when *ocaml-use-lsp* (add-hook 'reason-mode-hook 'lsp))
  :config
  ;; change the utop command to rtop
  (with-eval-after-load 'utop
    (when (executable-find "opam")
      (setq utop-command "opam config exec -- rtop -emacs"))))


(provide 'lang-ocaml)
;;; lang-ocaml.el ends here
