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


;; display certain documentation in the minibuffer
(use-package eldoc-mode
  :diminish (turn-on-eldoc-mode . "eldoc")
  :init (add-hook 'prog-mode-hook 'turn-on-eldoc-mode))


;; emmet mode for efficient xml/html entry
(use-package emmet-mode
  :ensure t
  :commands emmet-mode

  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-hook 'html-mode-hook 'emmet-mode)

  :config
  (setq emmet-indentation 2
        emmet-move-cursor-between-quotes t))


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
             flycheck-display-errors-function 'srb/flycheck-display-errors-function)))

  ;; flycheck
  (set-face-foreground 'flycheck-fringe-info "#268bd2"))


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


;; pandoc
(use-package pandoc-mode
  :ensure t
  :init (add-hook 'markdown-mode-hook 'pandoc-mode)
  :config (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))


;; hardcore parentheses management
(use-package paredit
  :ensure t
  :diminish (paredit-mode . "par")
  :defer t)


;; minor-mode and utility for regex conversion (perl <--> elisp)
(use-package pcre2el
  :ensure t
  :diminish (pcre-mode . "pcre")
  :init (pcre-mode +1))


;; project management and fast-switching
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config (projectile-global-mode t)
  (setq projectile-cache-file (expand-file-name  "projectile.cache" savefile-dir)))


;; displays colors for color hex values
(use-package rainbow-mode
  :ensure t
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode))


;; makes parentheses colorful
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

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode))


;; add a little configuration for xml files
(use-package nxml-mode
  :mode (("\\.xml\\'" . nxml-mode)
         ("\\.pom$" . nxml-mode))

  :config
  (setq nxml-child-indent 4
        nxml-attribute-indent 5
        nxml-auto-insert-xml-declaration-flag nil
        nxml-bind-meta-tab-to-complete-flag t
        nxml-slash-auto-complete-flag t)
  
  (add-hook 'nxml-mode-hook 'smartparens-mode)
  (add-hook 'nxml-mode-hook 'emmet-mode))


;; edit zsh/prezto files in sh-mode
(use-package sh-mode
  :config
  (defvar pretzo-files '("zlogin" "zlogin" "zlogout" "zpretzorc" "zprofile" "zshenv" "zshrc"))

  ;; have the mode launch for any prezto files
  (mapc (lambda (file)
          (add-to-list 'auto-mode-alist `(,(format "\\%s\\'" file) . sh-mode)))
        pretzo-files)

  ;; ;; mark the shell as zshell
  ;; (add-hook 'sh-mode-hook
  ;;           (lambda ()
  ;;             (if (and buffer-file-name
  ;;                      (member (file-name-nondirectory buffer-file-name) pretzo-files))
  ;;                 (sh-set-shell "zsh"))))

  ;; get a better "ll" setting
  (add-hook 'eshell-mode-hook
          (lambda ()
            ;; The 'ls' executable requires the Gnu version on the Mac
            (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                          "/usr/local/bin/gls"
                        "/bin/ls")))
              (defalias "ll" (concat ls " -AlohG --color=always")))))
  )


;; set up skewer browser REPL
(use-package skewer-mode
  :ensure t
  :commands skewer-mode run-skewer
  :disabled t
  :config (skewer-setup))


;; add subwords into yaml-mode
(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode))
  :commands yaml-mode
  :config (add-hook 'yaml-mode-hook 'subword-mode))


;; this function and list will download the appropriate major mode
;; when it encouters a language file that needs one
(defmacro lang-mode-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present.  The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

(defvar lang-mode-auto-install-alist
  '(("\\.cmake\\'" cmake-mode cmake-mode)
    ("CMakeLists\\.txt\\'" cmake-mode cmake-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.elm\\'" elm-mode elm-mode)
    ("\\.ex\\'" elixir-mode elixir-mode)
    ("\\.exs\\'" elixir-mode elixir-mode)
    ("\\.elixir\\'" elixir-mode elixir-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.kt\\'" kotlin-mode kotlin-mode)
    ("\\.kv\\'" kivy-mode kivy-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.ml\\'" tuareg tuareg-mode)
    ("\\.pp\\'" puppet-mode puppet-mode)
    ("\\.php\\'" php-mode php-mode)
    ("\\.proto\\'" protobuf-mode protobuf-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.rs\\'" rust-mode rust-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode scala-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.styl\\'" stylus-mode stylus-mode)
    ("\\.swift\\'" swift-mode swift-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.thrift\\'" thrift thrift-mode)
    ("\\.ts\\'" typescript-mode typescript-mode)
    ("Dockerfile\\'" dockerfile-mode dockerfile-mode)))

;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (lang-mode-auto-install extension package mode))))
 lang-mode-auto-install-alist)


(provide 'config-programming)
;;; config-programming.el ends here
