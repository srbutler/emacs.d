;;; lang-ocaml-lsp.el --- Summary:
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

  ;; for dune build files (not in melpa currently)
  (load-file "~/.opam/4.07.0/share/emacs/site-lisp/dune.el")

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
  :ensure t
  :bind (:map tuareg-mode-map ("C-c C-f" . ocp-indent-buffer))
  :init (add-hook 'tuareg-mode-hook 'ocp-setup-indent))


;; taken from spacemacs (package not in MELPA)
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
  :config
  ;; change the utop command to rtop
  (with-eval-after-load 'utop
    (when (executable-find "opam")
     (setq utop-command "opam config exec -- rtop -emacs"))))


(provide 'lang-ocaml-lsp)
;;; lang-ocaml-lsp.el ends here
