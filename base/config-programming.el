;;; package --- Summary:
;;
;;; Commentary:
;;
;;; Code:

;; autocompletion with company
(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0.5)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)

  :config
  (global-company-mode 1))


;; set up dash integration
(use-package dash-at-point
  :ensure t
  :init (global-set-key (kbd "C-c d") 'dash-at-point-with-docset)
  :config
  (add-to-list 'dash-at-point-mode-alist '(python-mode . "python3"))
  (add-to-list 'dash-at-point-mode-alist '(clojure-mode . "clojure"))
  (add-to-list 'dash-at-point-mode-alist '(sh-mode . "bash"))
  (add-to-list 'dash-at-point-mode-alist '(emacs-lisp-mode . "elisp"))
  (add-to-list 'dash-at-point-mode-alist '(ess-mode . "r"))
  (add-to-list 'dash-at-point-mode-alist '(LaTeX-mode . "latex"))
  (add-to-list 'dash-at-point-mode-alist '(js2-mode . "javascript"))
  (add-to-list 'dash-at-point-mode-alist '(haskell-mode . "haskell"))
  )


;; syntax-checking
(use-package flycheck
  :ensure t
  :init (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  ;; change flycheck's error display to only margin tick
  (eval-after-load 'flycheck
    '(progn
       (defun srb/flycheck-display-errors-function (errors)
         (mapc (lambda (err)
                 (message "FlyC: %s" (flycheck-error-message err)) (sit-for 1))
               errors))
       (setq flycheck-highlighting-mode nil
             flycheck-display-errors-function 'srb/flycheck-display-errors-function))))


;; have flycheck info appear in a popup
(use-package flycheck-pos-tip
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))


;; set up magit for git
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))


;; project management and fast-switching
(use-package projectile
  :ensure t
  :config (projectile-global-mode t)
  (setq projectile-cache-file (expand-file-name  "projectile.cache" savefile-dir)))


;; get smartparens in programming modes
(use-package smartparens
  :ensure t
  ;; smart pairing for all
  :init
  (progn
    (require 'smartparens-config)
    (setq sp-base-key-bindings         'paredit
          sp-autoskip-closing-pair     'always
          sp-hybrid-kill-entire-symbol nil)
    (sp-use-paredit-bindings))

  ;; turn on smartparens for all programming modes
  (add-hook 'prog-mode-hook 'smartparens-mode)
  :config
  (show-smartparens-global-mode +1))


;; define a bunch of wrapping operations in many modes
(use-package wrap-region
  :ensure t
  :diminish wrap-region-mode
  :config
  ;; (wrap-region-global-mode t)

  (add-hook 'text-mode-hook 'wrap-region-mode)
  (add-hook 'lisp-mode-hook 'wrap-region-mode)

  (wrap-region-add-wrappers
   '(("(" ")")
     ("[" "]")
     ("{" "}")
     ("<" ">")
     ("'" "'")
     ("\"" "\"")
     ("‘" "’"   "q")
     ("“" "”"   "Q")
     ("*" "*"   "b"   org-mode)                 ; bolden
     ("*" "*"   "*"   org-mode)                 ; bolden
     ("/" "/"   "i"   org-mode)                 ; italics
     ("/" "/"   "/"   org-mode)                 ; italics
     ("~" "~"   "c"   org-mode)                 ; code
     ("~" "~"   "~"   org-mode)                 ; code
     ("=" "="   "v"   org-mode)                 ; verbatim
     ("=" "="   "="   org-mode)                 ; verbatim
     ("_" "_"   "u" '(org-mode markdown-mode))  ; underline
     ("**" "**" "b"   markdown-mode)            ; bolden
     ("*" "*"   "i"   markdown-mode)            ; italics
     ;; ("`" "`"   "c" '(markdown-mode ruby-mode)) ; code
     ("`" "'"   "c"   lisp-mode)                ; code
 )))


;; enable YASnippet globally
(use-package yasnippet
  :ensure t
  :init (add-hook 'prog-mode-hook 'yas-minor-mode)
  :config (yas-load-directory "~/.emacs.d/snippets")
  ;;:diminish yas
  )

(provide 'config-programming)
;;; config-programming.el ends here
