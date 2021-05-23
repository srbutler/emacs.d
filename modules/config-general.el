;;; config-general.el -- Summary
;;
;;; Commentary:
;;
;;; Code:


;;;; SETTINGS

;; more useful frame title, that shows either a file or a buffer name
;; (if the buffer isn't visiting a file)
(setq frame-title-format
      '("emacs@" (:eval (system-name)) ": %* "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; setup savefiles/backups in a way that's not annoying
(setq backup-directory-alist `(("." . ,*savefile-dir*))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; set init GC frequency
(setq gc-cons-threshold (* 100 1000 1000))
;; increase frequency after init
(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold (* 8 1000 1000))))

;; set some basic defaults
(setq-default
 abbrev-file-name                (expand-file-name "abbrev_defs" *savefile-dir*)
 apropos-do-all                  t
 auto-save-default               t
 blink-matching-paren            t
 confirm-kill-emacs              'yes-or-no-p  ;; Confirm before exiting Emacs
 delete-active-region            t
 delete-by-moving-to-trash       t
 disabled-command-function       nil           ;; don't prompt for some disabled functions
 display-time-24hr-format        nil
 display-time-format             "%H:%M"       ;; Format the time string
 enable-local-variables          :all
 fill-column                     80
 ffap-machine-p-known            'reject       ;; stop attempts at pinging websites on autocomplete
 help-window-select              t             ;; Focus new help windows when opened
 indent-tabs-mode                nil           ;; Stop using tabs to indent
 indicate-empty-lines            nil
 inhibit-startup-message         t
 initial-scratch-message         ";; scratch\n"
 kill-do-not-save-duplicates     t
 large-file-warning-threshold    100000000     ;; warn when opening files bigger than 100MB
 linum-format                    " %4d "
 major-mode                      'text-mode
 mode-require-final-newline      t
 mouse-yank-at-point             t
 nsm-settings-file               (expand-file-name "network-security.data" *savefile-dir*)
 next-line-add-newlines          t             ;; adds newline for C-n at end of buffer
 require-final-newline           t
 ring-bell-function              'ignore
 scroll-preserve-screen-position t
 sentence-end-double-space       nil
 tab-always-indent               'complete     ;; smart tab behavior - indent or complete
 tab-width                       4
 truncate-lines                  t
 vc-follow-symlinks              t
 visible-bell                    t
 window-combination-resize       t             ;; Resize windows proportionally
 x-stretch-cursor                t             ;; stretch cursor for tab characters.
 )

(blink-cursor-mode 0)          ;; get rid of the blinking cursor
(column-number-mode t)         ;; put column number in mode-line
(delete-selection-mode)        ;; Replace region when inserting text
(fset 'yes-or-no-p 'y-or-n-p)  ;; shorten yes-or-no to y-or-n
(global-hl-line-mode +1)       ;; highlight the current line
(global-visual-line-mode 0)    ;; do not wrap long lines
(line-number-mode t)           ;; put column number in mode-line
(menu-bar-mode nil)            ;; display menu-bar in window only
(size-indication-mode t)
(tool-bar-mode 0)              ;; Disable the tool bar
(tooltip-mode 0)
(display-time-mode 0)          ;; Disable time in the mode-line

;; garbage collect when Emacs loses focus
;; (add-hook 'focus-out-hook 'garbage-collect)

;; remove the redundant scroll-bars
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; setup `hippie-expand' expand functions
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;;;; FUNCTIONS

;; defines the standard backtab behavior of most editors
(defun srb/un-indent-by-removing-4-spaces ()
  "Remove 4 spaces from beginning of of line."
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
        (replace-match "")))))


;; this is surprisingly useful
(defun srb/insert-buffer-name ()
  "Insert the buffer name into the buffer at the current editing point."
  (interactive "*")
  (insert (buffer-name)))


(defun srb/imenu-elisp-sections ()
  "Define imenu section headers with ';;;;'."
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression
               '("Sections" "^;;;; \\(.+\\)$" 1) t))
(add-hook 'emacs-lisp-mode-hook 'srb/imenu-elisp-sections)


;;;; BINDINGS

;; used in a few places to define keybindings easily
(use-package bind-key
  :bind (("C-z"      . nil)                ;; remove os minimization
         ("M-/"      . hippie-expand)      ;; try to complete a symbol
         ("<delete>" . delete-region)      ;; try and have a normal way to delete things
         ("C-c l"    . goto-line)          ;; go to line by number
         ("C-x m"    . menu-bar-mode)      ;; toggle the menu bar
         ("M-O"      . other-frame)        ;; jump to other frame
         ("M-c"      . capitalize-dwim)    ;; capitalize the word-at-point or region
         ("M-l"      . downcase-dwim)      ;; lowercase the word-at-point or region
         ("M-u"      . upcase-dwim)        ;; uppercase the word-at-point or region

         ;; custom functions
         ("<backtab>" . srb/un-indent-by-removing-4-spaces)))


;;;; BUILT-IN PACKAGES

;; revert buffers automatically when underlying files are changed externally
(use-package autorevert
  :config (global-auto-revert-mode t))


(use-package conf-mode
  :mode (("zathurarc\\'" . conf-space-mode)
         ("XCompose\\'"  . conf-colon-mode)
         ("dunstrc\\'"   . conf-toml-mode)
         ("sxhkdrc\\'"   . conf-space-mode))
  :hook (conf-mode . rainbow-mode))


;; C-native version of linum
(use-package display-line-numbers
  :when (version<= "26.0.50" emacs-version)
  :demand t
  :bind ("C-c C-l" . display-line-numbers-mode)
  :init
  (set-face-attribute 'line-number nil :height 0.9)
  (set-face-attribute 'line-number-current-line nil :height 0.9)
  :config
  (setq display-line-numbers-grow-only t)
  (global-display-line-numbers-mode t))


;; display certain documentation in the minibuffer
(use-package eldoc-mode
  :hook prog-mode
  :config
  ;; give current argument distinctive highlighting
  (set-face-attribute 'eldoc-highlight-function-argument nil
                      :underline t
                      :foreground (face-foreground font-lock-constant-face)
                      :weight 'bold))


(use-package prog-mode
  :hook (prog-mode . (lambda () (setq-local show-trailing-whitespace t)))
  :hook (prog-mode . electric-indent-mode)
  :hook (prog-mode . subword-mode))


;; save recent files
(use-package recentf
  :init
  (setq recentf-save-file (expand-file-name "recentf" *savefile-dir*)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  :config (recentf-mode +1))


;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :init
  (setq save-place-file (expand-file-name "saveplace" *savefile-dir*))
  ;; activate it for all buffers
  (setq-default save-place t))


;; savehist keeps track of some history
(use-package savehist
  :init
  (setq savehist-additional-variables '(search-ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file (expand-file-name "savehist" *savefile-dir*))
  (savehist-mode +1))


;; ensure sh-mode is setup for prezto files
(use-package sh-mode
  :mode (("\\.?profile\\'" . sh-mode)
         ("\\.?zlogin\\'" . sh-mode)
         ("\\.?zlogout\\'" . sh-mode)
         ("\\.?zpreztorc\\'" . sh-mode)
         ("\\.?zprofile\\'" . sh-mode)
         ("\\.?zshenv\\'" . sh-mode)
         ("\\.?zshrc\\'" . sh-mode))
  :init
  ;; install: npm i -g bash-language-server
  (when (executable-find "bash-language-server")
    (add-hook 'shell-mode-hook #'lsp))

  (when (executable-find "shellcheck")
    (with-eval-after-load 'flycheck
      (setq flycheck-sh-shellcheck-executable "/usr/bin/shellcheck")))
  )


;; meaningful names for buffers with the same name
(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))


;; indicates whitespace characters
(use-package whitespace-mode
  :bind ("C-c C-w" . whitespace-mode)
  :preface
  (defun srb/normalize-file ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (whitespace-cleanup)
      (delete-trailing-whitespace)
      (goto-char (point-max))
      (delete-blank-lines)
      (set-buffer-file-coding-system 'unix)
      (goto-char (point-min))
      (while (re-search-forward "\r$" nil t)
        (replace-match ""))
      (set-buffer-file-coding-system 'utf-8)
      (let ((require-final-newline t))
        (save-buffer)))))


;; use shift + arrow keys to switch between visible buffers
(use-package windmove
  :demand
  :bind (("M-S-<left>" . windmove-left)
         ("M-S-<right>" . windmove-right)
         ("M-S-<down>" . windmove-down)
         ("M-S-<up>" . windmove-up)))


;; ensure the initial xref commands are correctly bound
(use-package xref
  :bind (("M-." . xref-find-definitions)
         ("M-?" . xref-find-references)))


;;;; EXTERNAL PACKAGES

;; jump windows quickly, linked to key-chords below
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))


;; set up proper wrapping for text modes
(use-package adaptive-wrap
  :ensure t
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))


;; special handling for ansible yaml files
(use-package ansible
  :ensure t
  :after yaml-mode
  :hook (yaml-mode . check-enable-ansible)
  :preface
  (defvar ansible-filename-re
    ".*\\(main\.yml\\|site\.yml\\|encrypted\.yml\\|roles/.+\.yml\\|group_vars/.+\\|host_vars/.+\\)")

  (defun check-enable-ansible ()
    "Check to see if ansible-mode should be enabled for a YML buffer."
    (interactive)
    (when (and (stringp buffer-file-name)
               (string-match ansible-filename-re buffer-file-name))
      (ansible 1))))


(use-package ansible-doc
  :ensure t
  :after ansible
  :bind (:map ansible-key-map ("C-c D" . ansible-doc)))


;; quick jumps
(use-package avy
  :ensure t
  :demand t
  :bind ("M-j" . avy-goto-word-1))


;; autocompletion with company
(use-package company
  :ensure t
  :bind (:map company-active-map
              ("M-n" . nil)
              ("M-p" . nil)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay                0.5
        company-tooltip-limit             10
        company-minimum-prefix-length     2
        company-selection-wrap-around     t
        company-tooltip-flip-when-above   t
        company-tooltip-limit             14
        company-tooltip-align-annotations t
        company-require-match             'never
        company-backends                  '(company-capf)
        company-auto-complete             nil
        company-auto-complete-chars       nil
        company-dabbrev-other-buffers     nil
        company-dabbrev-ignore-case       nil
        company-dabbrev-downcase          nil
        company-frontends                 '(company-pseudo-tooltip-frontend))
  (global-company-mode 1))


;; icons for company
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))


;; rank completions based on usage
(use-package company-statistics
  :ensure t
  :after company
  :custom
  (company-statistics-file
   (expand-file-name "company-statistics-cache.el" *savefile-dir*))
  :config (company-statistics-mode))


;; set up some of crux's convenience functions
(use-package crux
  :ensure t
  :demand
  :bind (([remap kill-line] . crux-smart-kill-line)
         ([remap open-line] . crux-smart-open-line)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ("C-x 4 t"     . crux-transpose-windows)
         ("C-c C-x n"   . crux-cleanup-buffer-or-region)
         ("C-c C-x r"   . crux-rename-file-and-buffer)
         ("C-c C-x s"   . crux-sudo-edit)
         ("C-c C-x TAB" . crux-indent-rigidly-and-copy-to-clipboard))
  :bind (:map emacs-lisp-mode-map ("C-c e" . crux-eval-and-replace))
  :config
  ;; advice that modifies some general behavior
  ;; C-M-\ indents the whole file
  (crux-with-region-or-buffer indent-region)
  ;; tabify/untabify the whole buffer
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-buffer tabify))


(use-package csharp-mode
  :ensure t
  :hook lsp)


(use-package csv-mode
  :ensure t
  :mode (("\\.csv\\'" . csv-mode)
         ("\\.tsv\\'" . csv-mode)))


;; manage docker images
(use-package docker
  :ensure t)


(use-package docker-tramp
  :ensure t)


(use-package dockerfile-mode
  :ensure t
  :mode (("Dockerfile\\'" . dockerfile-mode)
         ("Containerfile\\'" . dockerfile-mode)))


;; R/Stata/etc.
(use-package ess
  :ensure ess)


;; get the PATH variable working correctly
(use-package exec-path-from-shell
  :when (memq window-system '(mac ns x))
  :ensure t
  :config (exec-path-from-shell-initialize))


;; expands the selection region progressively
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C-c =" . er/expand-region)))


;; syntax-checking
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)

  ;; constant rechecking gets annoying
  (setq flycheck-check-syntax-automatically '(mode-enabled save new-line))

  ;; get rid of annoying contrasts when using certain themes
  (set-face-background 'flycheck-fringe-info nil)
  (set-face-background 'flycheck-fringe-error nil)
  (set-face-background 'flycheck-fringe-warning nil))


;; have git indications in gutter
(use-package git-gutter-fringe
  ;; :when window-system
  :ensure t
  :init (global-git-gutter-mode t)
  :config
  ;; shrink values slightly
  (set-face-attribute 'git-gutter-fr:added nil :height 0.9)
  (set-face-attribute 'git-gutter-fr:deleted nil :height 0.9)
  (set-face-attribute 'git-gutter-fr:modified nil :height 0.9))


;; major mode for .gitconfig files
(use-package gitattributes-mode
  :ensure t
  :defer t)


;; major mode for .gitconfig files
(use-package gitconfig-mode
  :ensure t
  :defer t)


;; major mode for .gitignore files
(use-package gitignore-mode
  :ensure t
  :defer t)


(use-package gnuplot-mode
  :ensure t
  :mode (("\\.gpi\\'" . gnuplot-mode)
         ("\\.plt\\'" . gnuplot-mode)
         ("\\.gp\\'"  . gnuplot-mode)))


;; for Jenkinsfiles
(use-package groovy-mode
  :ensure t
  :mode ("Jenkinsfile" . groovy-mode))


;; adds guides to show indentation level
(use-package highlight-indent-guides
  :ensure t
  :config (setq highlight-indent-guides-method 'bitmap))


;; highlight TODO/FIXME/etc. comments
(use-package hl-todo
  :ensure t
  :config (global-hl-todo-mode))


;; make available for other packages for now
(use-package hydra
  :ensure t
  :config
  (defhydra hydra-zoom (global-map "C-c +")
    "zoom"
    ("+" text-scale-increase "in")
    ("=" text-scale-increase "in")
    ("-" text-scale-decrease "out")
    ("_" text-scale-decrease "out")))


;; for ini config files
(use-package ini-mode
  :ensure t)


;; for ansible templates
(use-package jinja2-mode
  :ensure t
  :mode ("\\.j2\\'" . jinja2-mode))


;; make language server protocol services available
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq lsp-auto-configure                 t
        lsp-enable-imenu                   t
        lsp-enable-indentation             t
        lsp-enable-xref                    t
        lsp-headerline-breadcrumb-enable   t
        lsp-headerline-breadcrumb-segments '(path-up-to-project file)
        lsp-keymap-prefix                  "C-c C-l"
        lsp-diagnostics-provider           :flycheck))


;; flycheck support and code previews/lenses
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ;; use the peek functions instead of jumps
              ("M-." . lsp-ui-peek-find-definitions)
              ("M-?" . lsp-ui-peek-find-references)
              ("C-c i" . lsp-ui-imenu)
              :map lsp-ui-peek-mode-map
              ("C-j" . lsp-ui-peek--goto-xref))
  :config
  (setq lsp-ui-doc-enable                 t
        lsp-ui-doc-max-width              75
        lsp-ui-sideline-delay             0.5
        lsp-ui-sideline-show-diagnostics  t
        lsp-ui-sideline-show-hover        nil  ;; disable repeated docs
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-ignore-duplicate  t))


;; set up magit for git
(use-package magit
  :ensure t
  :defer t
  :bind (("C-c g b" . magit-branch)
         ("C-c g B" . magit-blame)
         ("C-c g d" . magit-diff)
         ("C-c g f" . magit-fetch-all)
         ("C-c g F" . magit-file-dispatch)
         ("C-c g g" . magit-dispatch)
         ("C-c g l" . magit-log)
         ("C-c g m" . magit-merge)
         ("C-c g p" . magit-pull)
         ("C-c g P" . magit-push)
         ("C-c g r" . magit-rebase)
         ("C-c g R" . magit-reset)
         ("C-c g s" . magit-status)
         ("C-c g S" . magit-stash)
         ("C-c g v" . magit-revert)
         ("C-c g x" . magit-run))
  :config
  (setq magit-completing-read-function 'ivy-completing-read
        magit-diff-refine-hunk t))


;; display TODOs in status buffer
(use-package magit-todos
  :ensure t
  :hook (magit-mode . magit-todos-mode))


;; meson build scripts
(use-package meson-mode
  :ensure t)


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
  :when (executable-find "pandoc")
  :ensure t
  :hook (markdown-mode gfm-mode org-mode TeX-mode)
  :config (pandoc-load-default-settings))


;; minor-mode and utility for regex conversion (perl <--> elisp)
(use-package pcre2el
  :ensure t
  :init (pcre-mode +1))


;; workspace management
(use-package perspective
  :ensure t
  :config (persp-mode))


;; project management and fast-switching
(use-package projectile
  :ensure t
  :bind ("C-c p h" . projectile-find-other-file)
  :config
  (projectile-mode t)
  (setq projectile-cache-file
        (expand-file-name  "projectile.cache" *savefile-dir*))
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" *savefile-dir*)))


;; displays colors for color hex values
(use-package rainbow-mode
  :ensure t
  :hook (emacs-lisp-mode css-mode conf-colon-mode conf-space-mode sh-mode))


;; make parentheses colorful
(use-package rainbow-delimiters-mode
  :ensure rainbow-delimiters
  :hook (prog-mode comint-mode))


;; get smartparens in programming modes
(use-package smartparens
  :ensure t
  :bind (("M-s" . sp-unwrap-sexp))
  :hook ((prog-mode comint-mode) . smartparens-strict-mode)
  :init
  (use-package smartparens-config)
  (show-smartparens-global-mode 1)
  :config
  (sp-use-paredit-bindings)
  (setq sp-autoskip-closing-pair 'always
        sp-hybrid-kill-entire-symbol nil)

  ;; conflicts with xref
  (define-key smartparens-mode-map (kbd "M-?") nil)
  (define-key smartparens-mode-map (kbd "M-j") nil))


;; better line-by-line scrolling, especially in terminals
(use-package smooth-scrolling
  :ensure t
  :config (smooth-scrolling-mode 1))


;; visual undo history
(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-history-directory-alist `((".*" . ,*savefile-dir*))
        undo-tree-auto-save-history t
        undo-tree-visualizer-diff t)
  (global-undo-tree-mode))


;; unfill commandspp
(use-package unfill
  :ensure t
  :commands (unfill-region unfill-paragraph unfill-toggle)
  :bind (("C-M-Q" . unfill-toggle)
         ("M-Q" . unfill-paragraph))
  :config
  (defun srb/unfill-buffer ()
    "Unfill all paragraphs in a buffer"
    (interactive)
    (unfill-region (point-min) (point-max))))


;; cause I forget things
(use-package which-key
  :ensure t
  :init (which-key-mode 1))


;; define a bunch of wrapping operations in text modes
(use-package wrap-region
  :ensure t
  :hook ((org-mode markdown-mode text-mode) . wrap-region-mode)
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
     ("*" "*"   "b"   org-mode)       ;; bolden
     ("*" "*"   "*"   org-mode)       ;; bolden
     ("/" "/"   "i"   org-mode)       ;; italics
     ("/" "/"   "/"   org-mode)       ;; italics
     ("~" "~"   "c"   org-mode)       ;; code
     ("~" "~"   "~"   org-mode)       ;; code
     ("=" "="   "v"   org-mode)       ;; verbatim
     ("=" "="   "="   org-mode)       ;; verbatim
     ("_" "_"   "u"   org-mode)       ;; underline
     ("_" "_"   "u"   markdown-mode)  ;; underline
     ("**" "**" "b"   markdown-mode)  ;; bolden
     ("*" "*"   "i"   markdown-mode)  ;; italics
     ("`" "`"   "c"   markdown-mode)  ;; code
     ("~~" "~~" "s"   markdown-mode)  ;; strikethrough
     )))


;; unobtrusively trim trailing whitespace
(use-package ws-butler
  :ensure t
  :config (ws-butler-global-mode))


(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :hook ((yaml-mode . subword-mode)
         (yaml-mode . highlight-indent-guides-mode)))


;; enable YASnippet globally
(use-package yasnippet
  :ensure t
  :init (yas-global-mode)
  :bind (("C-c C-e" . yas-expand))
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets/" *dotfiles-dir*))

  ;; add to snippets to hippie expand
  (push 'yas-hippie-try-expand hippie-expand-try-functions-list))


;; a solid collection of snippets for many modes
(use-package yasnippet-snippets
  :ensure t)


(provide 'config-general)
;;; config-general.el ends here
