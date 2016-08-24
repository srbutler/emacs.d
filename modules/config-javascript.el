;;; package --- Summary:
;; config-javascript.el
;;
;;; Commentary:
;;
;;; Code:
;;

;; setup js2
(use-package js2-mode
  :ensure t
  :mode (("\\.jsx?$"  . js2-mode)
         ("\\.es6\\'" . js2-mode)
         ("\\.ejs\\'" . js2-mode)
         ("\\.pac\\'" . js2-mode))
  ;; :interpreter node
  :commands js2-mode
  :config
  (setq js-basic-indent 2
        mode-name "JS2")
  (setq-default js2-basic-offset 2
                js2-auto-indent-p t
                js2-cleanup-whitespace t
                js2-enter-indents-newline t
                js2-indent-on-enter-key t
                js2-global-externs (list "window" "module" "require" "buster" "sinon" "assert" "refute"
                                         "setTimeout" "clearTimeout" "setInterval""clearInterval" "location"
                                         "__dirname" "console" "JSON" "jQuery" "$"))

  (setq-local electric-layout-rules '((?\; . after)))    ;; prelude
  (js2-imenu-extras-mode +1)    ;; prelude
  (add-hook 'js2-mode-hook 'subword-mode))

  ;; set up refactoring, all prefixed to =C-c .=
  ;; (use-package js2-refactor
  ;;   :ensure t
  ;;   :commands js2r-add-keybindings-with-prefix
  ;;   :init   (add-hook 'js2-mode-hook 'js2-refactor-mode)
  ;;   :config (js2r-add-keybindings-with-prefix "C-c .")
  ;;   )

  ;; set up company
  ;; (add-to-list 'company-backends 'company-tern)

  ;; (use-package tern
  ;;   :ensure t
  ;;   :commands tern-mode
  ;;   :init (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
  ;;   :config
  ;;   ;; Setup Tern as an autocomplete source.
  ;;   (with-eval-after-load "company"
  ;;     (use-package company-tern
  ;;       :init (add-to-list 'company-backends 'company-tern)))))

;; for js2-comint, to call node.js as a REPL
;; (use-package js-comint
;;   :ensure t
;;   :defer t
;;   :init
;;   (setq inferior-js-program-command "node")
;;   (setq inferior-js-program-arguments '("--interactive"))

;;   (add-hook 'js2-mode-hook '(lambda ()
;;                               (local-set-key "\C-x\C-e" 'js-send-last-sexp)
;;                               (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
;;                               (local-set-key "\C-cb" 'js-send-buffer)
;;                               (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
;;                               (local-set-key "\C-cl" 'js-load-file-and-go)
;;                               ))

;;   (setenv "NODE_NO_READLINE" "1")
;;   (setq inferior-js-mode-hook
;;         (lambda ()
;;           ;; We like nice colors
;;           (ansi-color-for-comint-mode-on)))
;;   )

;; set up for coffeescript
;; (use-package coffee-mode
;;   :ensure t
;;   :mode "\\.coffee\'"
;;   :init
;;   (progn
;;     (add-hook 'coffee-mode-hook
;; 	      '(lambda ()
;; 		 (setq tab-width 4
;; 		       coffee-tab-width 4)
;; 		 (local-set-key (kbd "C-c C-c") 'coffee-compile-buffer)))
;;     (add-hook 'coffee-mode-hook 'subword-mode)
;;     )
;;   )

;;(use-package skewer-mode
 ;; :ensure t
  ;; :init (add-hook 'js2-mode-hook 'skewer-mode)
 ;; )

;; config-javascript.el ends here
