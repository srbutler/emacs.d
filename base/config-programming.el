;;; config-programming.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

;; PROGRAMMING HELPER MODES -------------------------

;; autocompletion with company
(use-package company
  :ensure t
  :diminish (company-mode . "comp")
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
  :diminish (flycheck-mode . "flyc")
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


;; have git indications in gutter
(use-package git-gutter
  :ensure t
  :init (global-git-gutter-mode t)
  :diminish git-gutter-mode
  :config
  ;; git-gutter
  (set-face-foreground 'git-gutter:added "#859900")
  (set-face-foreground 'git-gutter:deleted "#dc322f")
  (set-face-foreground 'git-gutter:modified "#b58900")
  )


;; set up magit for git
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))


;; hardcore parentheses management
(use-package paredit
  :ensure t
  :diminish (paredit-mode . "par")
  :defer t)


;; project management and fast-switching
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config (projectile-global-mode t)
  (setq projectile-cache-file (expand-file-name  "projectile.cache" savefile-dir)))


(use-package rainbow-mode
  :ensure t)


(use-package rainbow-delimiters-mode
  :config (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode))


;; get smartparens in programming modes
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
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

;;; OTHER MODES ------------------------------------------
;; these are language modes that don't need their own file

;; emmet mode for efficient entry
(use-package emmet-mode
  :ensure t
  :commands emmet-mode
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-hook 'html-mode-hook 'emmet-mode)
  (add-hook 'nxml-mode-hook 'emmet-mode)
  :config
  (setq emmet-indentation 2
        emmet-move-cursor-between-quotes t)
  )

;; add a little configuration for xml files
(use-package nxml-mode
  :mode (("\\.xml" . nxml-mode)
         ("\\.pom$" . nxml-mode))
  :config
  (setq nxml-child-indent 4
        nxml-attribute-indent 5
        nxml-auto-insert-xml-declaration-flag nil
        nxml-bind-meta-tab-to-complete-flag t
        nxml-slash-auto-complete-flag t))


;; pandoc
(use-package pandoc-mode
  :ensure t
  :init (add-hook 'markdown-mode-hook 'pandoc-mode)
  :config (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))


;; utility for regex conversion
(use-package pcre2el
  :ensure t
  :defer t)


;; edit zsh/prezto files in sh-mode
(use-package sh-mode
  :config
  (defvar pretzo-files '("zlogin" "zlogin" "zlogout" "zpretzorc" "zprofile" "zshenv" "zshrc"))

  ;; have the mode launch for any prezto files
  (mapc (lambda (file)
          (add-to-list 'auto-mode-alist `(,(format "\\%s\\'" file) . sh-mode)))
        pretzo-files)

  ;; mark the shell as zshell
  (add-hook 'sh-mode-hook
            (lambda ()
              (if (and buffer-file-name
                       (member (file-name-nondirectory buffer-file-name) pretzo-files))
                  (sh-set-shell "zsh"))))
  )

;; set up skewer browser REPL
(use-package skewer-mode
  :ensure t
  :commands skewer-mode run-skewer
  :disabled t
  :config (skewer-setup))


;; add subwords into yaml-mode
(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook 'subword-mode))

(provide 'config-programming)
;;; config-programming.el ends here
