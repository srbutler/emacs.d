;;; config-ui.el -- Summary
;;
;;; Commentary:
;;
;;; Code:


;; more useful frame title, that shows either a file or a buffer name
;; (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name ": " (:eval (if (buffer-file-name)
                                           (abbreviate-file-name (buffer-file-name))
                                         "%b"))))

;; set some basic defaults
(setq-default
 abbrev-file-name                "~/.emacs.d/savefile/abbrev_defs"
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


;; revert buffers automatically when underlying files are changed externally
(use-package autorevert
  :diminish (auto-revert-mode . "ar")
  :config (global-auto-revert-mode t))


;; linked to key-chords below
(use-package avy
  :ensure t)


;; used in a few places to define keybindings easily
(use-package bind-key)


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


;; diminish keeps the modeline tidy
(use-package diminish)


;; expands the selection region progressively
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))


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


;; displays colors for color hex values
(use-package rainbow-mode
  :ensure t
  :hook (emacs-lisp-mode css-mode)
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


;; visual undo history
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-visualizer-diff t)
  :config (global-undo-tree-mode))


;; unfill commands
(use-package unfill
  :ensure t
  :commands (unfill-region unfill-paragraph unfill-toggle)
  :bind ("M-Q" . unfill-paragraph))


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
  :diminish wrap-region-mode
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


(provide 'config-ui)
;;; config-ui.el ends here
