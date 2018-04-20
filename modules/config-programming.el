;;; config-programming.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

;; autocompletion with company
(use-package company
  :ensure t
  :diminish (company-mode . "comp")
  :custom
  (company-idle-delay 0.5)
  (company-tooltip-limit 10)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-tooltip-flip-when-above t)
  :config
  (global-company-mode 1)

  ;; remap select-next/prev to use normal up/down
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)))

;; rank completions based on usage
(use-package company-statistics
  :ensure t
  :after company
  :custom
  (company-statistics-file
   (expand-file-name "company-statistics-cache.el" *savefile-dir*))
  :config (company-statistics-mode))


;; set up dash integration
(use-package dash-at-point
  :disabled
  :ensure t
  :defer t
  :config
  (dolist
      (pair
       '('(python-mode . "python3") '(sh-mode . "bash") '(emacs-lisp-mode . "elisp")
         '(LaTeX-mode . "latex") '(js2-mode . "javascript") '(tuareg-mode . "ocaml")
         '(ess-mode . "r")))
    (add-to-list 'dash-at-point-mode-alist pair)))


;; https://www.masteringemacs.org/article/working-multiple-files-dired
(use-package find-file
  :init (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))


;; display certain documentation in the minibuffer
(use-package eldoc-mode
  :ensure nil
  :diminish ""
  :hook prog-mode
  :config
  ;; give current argument distinctive highlighting
  (set-face-attribute 'eldoc-highlight-function-argument nil
                      :underline t
                      :foreground (face-foreground font-lock-constant-face)
                      :weight 'bold))


;; syntax-checking
(use-package flycheck
  :ensure t
  :diminish (flycheck-mode . "flyc")
  :config
  (global-flycheck-mode)

  (setq flycheck-check-syntax-automatically '(mode-enabled save new-line))

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


;; make language server protocol services available
;; should be deferred until a service that needs it calls
(use-package lsp-mode
  :ensure t
  :defer t)


;; link lsp output with company
(use-package company-lsp
  :after (lsp-mode company)
  :custom
  (company-lsp-enable-recompletion t)
  (company-lsp-async t)
  (company-lsp-enable-snippet t)
  :init (add-to-list 'company-backends 'company-lsp))


;; let LSP work with imenu
(use-package lsp-imenu
  :ensure nil
  :after lsp-mode
  :init (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))


;; edit with multiple cursors
(use-package multiple-cursors
  :ensure t
  :config (setq mc/list-file (expand-file-name "mc-lists.el" *savefile-dir*))
  :bind
   (("C-c m t" . mc/mark-all-like-this)
    ("C-c m m" . mc/mark-all-like-this-dwim)
    ("C-c m l" . mc/edit-lines)
    ("C-c m e" . mc/edit-ends-of-lines)
    ("C-c m a" . mc/edit-beginnings-of-lines)
    ("C-c m n" . mc/mark-next-like-this)
    ("C-c m p" . mc/mark-previous-like-this)
    ("C-c m s" . mc/mark-sgml-tag-pair)
    ("C-c m d" . mc/mark-all-like-this-in-defun)))


;; pandoc
(use-package pandoc-mode
  :ensure t
  :diminish (pandoc-mode . "pandoc")
  :hook (markdown-mode org-mode TeX-mode)
  :config (pandoc-load-default-settings))


;; hardcore parentheses management
(use-package paredit
  :ensure t
  :defer t
  :diminish (paredit-mode . "par")
  :init
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'scheme-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode))


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
  (setq projectile-cache-file (expand-file-name  "projectile.cache" *savefile-dir*)))


;; makes parentheses colorful
(use-package rainbow-delimiters-mode
  :ensure rainbow-delimiters
  :hook (prog-mode))


;; get smartparens in programming modes
(use-package smartparens
  :ensure t
  :diminish (smartparens-mode . "sp")
  :init
  (use-package smartparens-config :ensure nil)
  (sp-use-paredit-bindings)
  ;; (smartparens-global-mode 1)
  (add-hook 'prog-mode-hook 'smartparens-mode)
  ;; (show-smartparens-global-mode 1)
  :config

  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
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


;; enable YASnippet globally
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode)
  :bind (("C-c C-e" . yas-expand))
  :config (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/"))


;; a solid collection of snippets for many modes
(use-package yasnippet-snippets
  :ensure t)


;;; OTHER MODES ------------------------------------------
;; these are language modes that don't need their own file

(use-package csv-mode
  :ensure t
  :mode (("\\.csv\\'" . csv-mode)
         ("\\.tsv\\'" . csv-mode)))


(use-package gnuplot-mode
  :ensure t
  :mode (("\\.gpi\\'" . gnuplot-mode)
         ("\\.plt\\'" . gnuplot-mode)
         ("\\.gp\\'"  . gnuplot-mode)))


;; personal mode for phoenix grammars
(use-package phoenix-grammar-mode
  :mode ("\\.gra\\'" . phoenix-grammar-mode)
  :load-path "~/.emacs.d/vendor/phoenix-grammar-mode")


;; edit zsh/prezto files in sh-mode
(defvar pretzo-files '("zlogin" "zlogout" "zpretzorc"
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
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.kt\\'" kotlin-mode kotlin-mode)
    ("\\.kv\\'" kivy-mode kivy-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.pp\\'" puppet-mode puppet-mode)
    ("\\.php\\'" php-mode php-mode)
    ("\\.proto\\'" protobuf-mode protobuf-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.sass\\'" sass-mode sass-mode)
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
