;;; lang-ocaml.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

;; Ocaml major mode
(use-package tuareg
  :ensure t
  :ensure-system-package opam
  :mode (("\\.ml[ily]?$"  . tuareg-mode)
         ("\\.topml$"     . tuareg-mode)
         ("\\.ocamlinit$" . tuareg-mode)
         ("jbuild$"       . tuareg-dune-mode))

  :init
  ;; Make OCaml-generated files invisible to filename completion
  (dolist
      (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi" ".cmxs" ".cmt" ".cmti" ".annot"))
    (add-to-list 'completion-ignored-extensions ext))

  :config
  ;; disable backtick/single-quote pairing
  (sp-local-pair 'tuareg-mode "'" nil :actions nil)
  (sp-local-pair 'tuareg-mode "`" nil :actions nil))


(use-package lsp-ocaml
  :ensure t
  :ensure-system-package
  ((ocaml-language-server . "npm i -g ocaml-language-server")
   (ocamlmerlin . "opam install merlin"))
  :hook ((tuareg-mode caml-mode reason-mode) . lsp-ocaml-enable))


;; completion engine
(use-package merlin
  :disabled t
  :ensure t
  :ensure-system-package (ocamlmerlin . "opam install merlin")
  :defer t
  :after tuareg company
  :bind (:map tuareg-mode-map
         ("C-c C-t" . merlin-type-enclosing))
  :init
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (add-hook 'reason-mode-hook 'merlin-mode)

  (setq merlin-completion-with-doc t)

  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'merlin-company-backend)))


;; error checking
(use-package flycheck-ocaml
  :disabled t
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
  :ensure-system-package (utop . "opam install utop")
  :defer t
  :init
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  (add-hook 'reason-mode-hook 'utop-minor-mode)
  :config
  (when (executable-find "opam")
    (setq utop-command "opam config exec -- utop -emacs")))


;; indenting/formatting
(use-package ocp-indent
  :after tuareg-mode
  :ensure t
  :ensure-system-package (ocp-indent . "opam install ocp-indent")
  :defer t
  :bind (:map tuareg-mode-map ("C-c C-f" . ocp-indent-buffer))
  :init (add-hook 'tuareg-mode-hook 'ocp-setup-indent))


;; taken from spacemacs (package not in MELPA)
(use-package merlin-eldoc
  :defer t
  :load-path "~/.emacs.d/vendor/merlin-eldoc/merlin-eldoc.el"
  :init (add-hook 'merlin-mode-hook 'merlin-eldoc/setup))


;; for using Reason's syntax instead of OCaml's
(use-package reason-mode
  :ensure t
  :mode ("\\.rei?$" . reason-mode)
  :config
  ;; change the utop command to rtop
  (with-eval-after-load 'utop
    (when (executable-find "opam")
     (setq utop-command "opam config exec -- rtop -emacs"))))


;; for making #doc calls in utop
(use-package ocp-index
  :defer t
  :load-path "~/.opam/4.05.0/share/emacs/site-lisp")


(provide 'lang-ocaml)
;;; lang-ocaml.el ends here
