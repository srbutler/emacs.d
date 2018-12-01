;;; config-ui.el -- Summary
;;
;;; Commentary:
;;
;;; Code:


;; more useful frame title, that shows either a file or a buffer name
;; (if the buffer isn't visiting a file)
(setq frame-title-format
      '("emacs@" (:eval (system-name)) " -- "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; set some basic defaults
(setq-default
 abbrev-file-name                (expand-file-name "abbrev_defs" *savefile-dir*)
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
 linum-format                    " %4d "
 major-mode                      'text-mode
 mode-require-final-newline      t
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
(display-time-mode)            ;; Enable time in the mode-line
(fset 'yes-or-no-p 'y-or-n-p)  ;; shorten yes-or-no to y-or-n
(global-visual-line-mode 0)    ;; do not wrap long lines
(line-number-mode t)           ;; put column number in mode-line
(size-indication-mode t)

(when window-system
  (global-hl-line-mode +1)    ;; highlight the current line
  (tool-bar-mode 0)           ;; Disable the tool bar
  (tooltip-mode 0))           ;; Disable the tooltips

;; If launching Emacs as in windowing system, show the menu. If
;; launching in a tty/terminal window, don't display the menu.
(if window-system
    (menu-bar-mode t)
  (menu-bar-mode -1))

;; remove the redundant scroll-bars
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; show whitespace in prog-modes only
(add-hook 'prog-mode-hook
          #'(lambda () (setq-local show-trailing-whitespace t)))

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


;; used in a few places to define keybindings easily
(use-package bind-key)

;; auto-indent on Enter
;; bind to prog-mode-map for now to not break helm, etc.
(bind-key "<return>" #'newline-and-indent prog-mode-map)

;; try and have a normal way to delete things
(bind-key "<delete>" #'delete-region global-map)

;; set an extra command to jump to other window, for convenience
(bind-key "M-o" #'other-window global-map)

;; set a general key for goto-line
(bind-key "C-c l" #'goto-line global-map)


;; defines the standard backtab behavior of most editors
(defun un-indent-by-removing-4-spaces ()
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
(bind-key "<backtab>" #'un-indent-by-removing-4-spaces global-map)

;; ensure proper bracket handling happens outside of smartparens
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'electric-indent-mode)


;; edit zsh/prezto files in sh-mode
(defvar pretzo-files '("zlogin" "zlogout" "zpretzorc"
                       "zprofile" "zshenv" "zshrc"))
(mapc
 (lambda (file)
   (add-to-list 'auto-mode-alist `(,(format "\\%s\\'" file) . sh-mode)))
 pretzo-files)


;; revert buffers automatically when underlying files are changed externally
(use-package autorevert
  :diminish (auto-revert-mode . "ar")
  :config (global-auto-revert-mode t))


;; linked to key-chords below
(use-package avy
  :ensure t)


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


;; set up some of crux's convenience functions
(use-package crux
  :ensure t
  :demand
  :bind (("C-c e"   . crux-eval-and-replace)
         ("C-x 4 t" . crux-transpose-windows)
         ("C-c I"   . crux-find-user-init-file)
         ("C-c S"   . crux-find-shell-init-file)
         ("C-c n"   . crux-cleanup-buffer-or-region))
  :config
  ;; kills to end of line first, then whole line
  (global-set-key [remap kill-line] #'crux-smart-kill-line)
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  (global-set-key [remap open-line] #'crux-smart-open-line)
  (global-set-key [remap kill-whole-line] #'crux-kill-whole-line)

  ;; advice that modifies some general behavior
  ;; C-M-\ indents the whole file
  (crux-with-region-or-buffer indent-region)
  ;; tabify/untabify the whole buffer
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-buffer tabify))


(use-package csv-mode
  :ensure t
  :mode (("\\.csv\\'" . csv-mode)
         ("\\.tsv\\'" . csv-mode)))


;; set up dash integration
(use-package dash-at-point
  :disabled t
  :ensure t
  :defer t
  :config
  (dolist
      (pair
       '('(python-mode . "python3") '(sh-mode . "bash") '(emacs-lisp-mode . "elisp")
         '(LaTeX-mode . "latex") '(js2-mode . "javascript") '(tuareg-mode . "ocaml")
         '(ess-mode . "r")))
    (add-to-list 'dash-at-point-mode-alist pair)))


;; diminish keeps the modeline tidy
(use-package diminish
  :ensure t)


(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode))



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



;; expands the selection region progressively
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))


;; https://www.masteringemacs.org/article/working-multiple-files-dired
(use-package find-file
  :init (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))


;; syntax-checking
(use-package flycheck
  :ensure t
  :ensure-system-package (cppcheck shellcheck)
  :diminish (flycheck-mode . "flyc")
  :config
  (global-flycheck-mode)

  ;; constant rechecking gets annoying
  (setq flycheck-check-syntax-automatically '(mode-enabled save new-line))

  ;; get rid of annoying contrasts when using certain themes
  (set-face-background 'flycheck-fringe-info nil)
  (set-face-background 'flycheck-fringe-error nil)
  (set-face-background 'flycheck-fringe-warning nil))


;; have git indications in gutter
(use-package git-gutter
  :ensure t
  :init (global-git-gutter-mode t)
  :diminish git-gutter-mode
  :config
  ;; change the indicator colors to something nicer
  (when (or (eq *current-theme-name* "solarized-dark")
            (eq *current-theme-name* "solarized-light"))
    (progn
      (set-face-foreground 'git-gutter:added "#859900")
      (set-face-foreground 'git-gutter:deleted "#dc322f")
      (set-face-foreground 'git-gutter:modified "#b58900"))))


;; navigate through git commit history
(use-package git-timemachine
  :ensure t
  :defer t)


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



;; define a bunch of quick key combos for basic actions
(use-package key-chord
  :ensure t
  :after avy
  :init (key-chord-mode +1)
  :config
  ;; quick avy calls
  (key-chord-define-global "jj" 'avy-goto-word-1)
  (key-chord-define-global "jl" 'avy-goto-line)
  (key-chord-define-global "jk" 'avy-goto-char))


;; set up magit for git
(use-package magit
  :ensure t
  :defer t
  :custom (magit-completing-read-function 'ivy-completing-read)
  :bind (("C-c g l"   . magit-log-popup)
         ("C-c g p s" . magit-push-popup)
         ("C-c g p l" . magit-pull-and-fetch-popup)
         ("C-c g r"   . magit-rebase-popup)
         ("C-c g s"   . magit-status)))


;; display TODOs in status buffer
(use-package magit-todos
  :ensure t
  :hook (magit-mode . magit-todos-mode))


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
  :ensure-system-package pandoc
  :diminish (pandoc-mode . "pandoc")
  :hook (markdown-mode org-mode TeX-mode)
  :config (pandoc-load-default-settings))


;; minor-mode and utility for regex conversion (perl <--> elisp)
(use-package pcre2el
  :ensure t
  :diminish (pcre-mode . "pcre")
  :init (pcre-mode +1))


;; workspace management
(use-package perspective
  :ensure t
  :config (persp-mode))


;; project management and fast-switching
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-mode t)
  (setq projectile-cache-file
        (expand-file-name  "projectile.cache" *savefile-dir*))
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" *savefile-dir*)))


;; makes parentheses colorful
(use-package rainbow-delimiters-mode
  :ensure rainbow-delimiters
  :hook (prog-mode))


;; displays colors for color hex values
(use-package rainbow-mode
  :ensure t
  :hook (emacs-lisp-mode css-mode conf-xdefaults-mode)
  :diminish rainbow-mode)


;; save recent files
(use-package recentf
  :init
  (setq recentf-save-file (expand-file-name "recentf" *savefile-dir*)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))


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
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" *savefile-dir*))
  (savehist-mode +1))


;; get smartparens in programming modes
(use-package smartparens
  :ensure t
  :diminish (smartparens-mode . "sp")
  :init
  (use-package smartparens-config)
  (sp-use-paredit-bindings)
  (add-hook 'prog-mode-hook 'smartparens-strict-mode)
  (show-smartparens-global-mode 1)
  :config
  (setq sp-base-key-bindings 'paredit
        sp-autoskip-closing-pair 'always
        sp-hybrid-kill-entire-symbol nil)

  (sp-local-pair '(markdown-mode gfm-mode) "*" "*"
                 :unless '(sp-in-string-p)
                 :actions '(insert wrap))
  :config
  ;; conflicts with xref
  (define-key smartparens-mode-map (kbd "M-?") nil))


;; better line-by-line scrolling, especially in terminals
(use-package smooth-scrolling
  :ensure t
  :config (smooth-scrolling-mode 1))


;; visual undo history
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-visualizer-diff t)
  :config
  (setq undo-tree-history-directory-alist `(("." . ,*savefile-dir*)))
  (global-undo-tree-mode))


;; unfill commands
(use-package unfill
  :ensure t
  :commands (unfill-region unfill-paragraph unfill-toggle)
  :bind (("C-M-Q" . unfill-toggle)
         ("M-Q" . unfill-paragraph)))


;; meaningful names for buffers with the same name
(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))


;; cause I forget things
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode 1))


;; use shift + arrow keys to switch between visible buffers
(use-package windmove
  :demand
  :bind (("M-S-<left>" . windmove-left)
         ("M-S-<right>" . windmove-right)
         ("M-S-<down>" . windmove-down)
         ("M-S-<up>" . windmove-up)))


;; define a bunch of wrapping operations in text modes
(use-package wrap-region
  :ensure t
  :hook ((org-mode . wrap-region-mode)
         (markdown-mode . wrap-region-mode)
         (text-mode . wrap-region-mode))
  :diminish (wrap-region-mode . "wrap")
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
     )))


;; unobtrusively trim trailing whitespace
(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config (ws-butler-global-mode))


;; ensure the initial xref commands are correctly bound
(use-package xref
  :bind (("M-." . xref-find-definitions)
         ("M-?" . xref-find-references)
         ("M-/" . xref-find-references)))


(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode))
  :commands yaml-mode
  :config (add-hook 'yaml-mode-hook 'subword-mode))


;; enable YASnippet globally
(use-package yasnippet
  :ensure t
  :init (yas-global-mode)
  :bind (("C-c C-e" . yas-expand))
  :config (add-to-list 'yas-snippet-dirs
                       (expand-file-name "snippets/" *dotfiles-dir*)))


;; a solid collection of snippets for many modes
(use-package yasnippet-snippets
  :ensure t)


(provide 'config-ui)
;;; config-ui.el ends here