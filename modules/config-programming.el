;;; config-programming.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

;; autocompletion with company
(use-package company
  :ensure t
  :diminish (company-mode . "comp")
  :init
  (setq company-idle-delay            0.5
        company-tooltip-limit         10
        company-minimum-prefix-length 2
        
        ;; invert the navigation direction if the the completion popup-isearch-match
        ;; is displayed on top (happens near the bottom of windows)
        company-tooltip-flip-when-above t)

  :config (global-company-mode 1))


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
  :diminish (eldoc-mode . "eldoc")
  :init (add-hook 'prog-mode-hook 'eldoc-mode))


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

  ;; get rid of annoying contrasts when using certain themes
  (set-face-background 'flycheck-fringe-info nil)
  (set-face-background 'flycheck-fringe-error nil)
  (set-face-background 'flycheck-fringe-warning nil)

  ;; change the info color when using solarized
  (when (or (eq current-theme-name "solarized-dark")
            (eq current-theme-name "solarized-light"))
    (set-face-foreground 'flycheck-fringe-info "#268bd2")))


;; have flycheck info appear in a popup
(use-package flycheck-pos-tip
  :disabled t
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
  
  ;; change the indicator colors to something nicer
  (when (or (eq current-theme-name "solarized-dark")
            (eq current-theme-name "solarized-light"))
    (progn
      (set-face-foreground 'git-gutter:added "#859900")
      (set-face-foreground 'git-gutter:deleted "#dc322f")
      (set-face-foreground 'git-gutter:modified "#b58900"))))


;; set up magit for git
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))


;; pandoc
(use-package pandoc-mode
  :ensure t
  :diminish (pandoc-mode . "pandoc")
  :init
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  (add-hook 'org-mode-hook 'pandoc-mode)
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
  :config
  (projectile-mode t)
  (setq projectile-cache-file (expand-file-name  "projectile.cache" savefile-dir)))


;; displays colors for color hex values
(use-package rainbow-mode
  :ensure t
  :defer t
  :diminish (rainbow-mode . "rbow")
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
    (sp-use-paredit-bindings))

  ;; turn on smartparens for all programming modes
  (add-hook 'prog-mode-hook 'smartparens-mode)
  
  :config
  (setq sp-base-key-bindings          'paredit
        sp-autoskip-closing-pair      'always
        sp-hybrid-kill-entire-symbol  nil)
  (show-smartparens-global-mode +1)

  ;; smartparens defaults taken from graphene, to make bracket handling
  ;; a little better
  (defun graphene--sp-pair-on-newline (id action context)
    "Put trailing pair on newline and return to point."
    (save-excursion
      (newline)
      (indent-according-to-mode)))

  (defun graphene--sp-pair-on-newline-and-indent (id action context)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    (graphene--sp-pair-on-newline id action context)
    (indent-according-to-mode))
  
  (sp-pair "{" nil :post-handlers
           '(:add ((lambda (id action context)
                     (graphene--sp-pair-on-newline-and-indent id action context)) "RET")))
  
  (sp-pair "[" nil :post-handlers
           '(:add ((lambda (id action context)
                     (graphene--sp-pair-on-newline-and-indent id action context)) "RET")))
  
  (sp-local-pair '(markdown-mode gfm-mode) "*" "*"
                 :unless '(sp-in-string-p)
                 :actions '(insert wrap)))


;; define a bunch of wrapping operations in text modes
(use-package wrap-region
  :ensure t
  :diminish wrap-region-mode
  :init (add-hook 'text-mode-hook 'wrap-region-mode)
  :config
  (wrap-region-add-wrappers
   '(("(" ")")
     ("[" "]")
     ("{" "}")
     ("<" ">")
     ("'" "'")
     ("\"" "\"")
     ("‘" "’"   "q")
     ("“" "”"   "Q")
     ("*" "*"   "b"   org-mode)                 ;; bolden
     ("*" "*"   "*"   org-mode)                 ;; bolden
     ("/" "/"   "i"   org-mode)                 ;; italics
     ("/" "/"   "/"   org-mode)                 ;; italics
     ("~" "~"   "c"   org-mode)                 ;; code
     ("~" "~"   "~"   org-mode)                 ;; code
     ("=" "="   "v"   org-mode)                 ;; verbatim
     ("=" "="   "="   org-mode)                 ;; verbatim
     ("_" "_"   "u"   org-mode)                 ;; underline
     ("_" "_"   "u"   markdown-mode)            ;; underline
     ("**" "**" "b"   markdown-mode)            ;; bolden
     ("*" "*"   "i"   markdown-mode)            ;; italics
     ("`" "`"   "c"   markdown-mode)            ;; code
     )))


;; enable YASnippet globally
(use-package yasnippet
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode)
  :config (yas-load-directory "~/.emacs.d/snippets")
  )


;;; FUNCTIONS --------------------------------------------

;; taken from graphene; inserts ; at end of line
(defun insert-semicolon-at-end-of-line ()
  "Add a closing semicolon from anywhere in the line."
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ";")))
(global-set-key (kbd "C-;") 'insert-semicolon-at-end-of-line)


;;; OTHER MODES ------------------------------------------
;; these are language modes that don't need their own file

(use-package csv-mode
  :ensure t
  :mode (("\\.csv\\'" . csv-mode)
         ("\\.tsv\\'" . csv-mode)))

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode))


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


;; personal mode for phoenix grammars
(use-package phoenix-grammar-mode
  :mode ("\\.gra\\'" . phoenix-grammar-mode)
  :load-path "~/.emacs.d/vendor/phoenix-grammar-mode")


;; edit zsh/prezto files in sh-mode
(defvar pretzo-files '("zlogin" "zlogin" "zlogout" "zpretzorc"
                       "zprofile" "zshenv" "zshrc"))
(mapc
 (lambda (file)
   (add-to-list 'auto-mode-alist `(,(format "\\%s\\'" file) . sh-mode)))
 pretzo-files)


;; for thrax/opengrm grammars (.grm)
(use-package thrax-mode
  :mode ("\\.grm\\'" . thrax-mode)
  :load-path "~/.emacs.d/vendor/thrax-mode/")


;; add subwords into yaml-mode
(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode))
  :commands yaml-mode
  :config (add-hook 'yaml-mode-hook 'subword-mode))


;; this macro and list will download the appropriate major mode
;; when it encouters a language file that needs one
;; taken from emacs-prelude
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
