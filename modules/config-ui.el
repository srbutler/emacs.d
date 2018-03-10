;;; config-ui.el -- Summary
;;
;;; Commentary:
;;
;;; Code:


;; put my user info
(setq user-full-name "Steven Butler"
      user-mail-address "srbutler@gmail.com")

;; more useful frame title, that show either a file or a buffer name
;; (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name ": " (:eval (if (buffer-file-name)
                                           (abbreviate-file-name (buffer-file-name))
                                         "%b"))))

;; shorten yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; get rid of the blinking cursor
(blink-cursor-mode -1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; highlight the current line
(if window-system
    (global-hl-line-mode +1))

;; delete on selection
(add-hook 'prog-mode-hook 'delete-selection-mode)

;; setup `hippie-expand' expand functions
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; set some basic defaults
(setq-default abbrev-file-name                "~/.emacs.d/savefile/abbrev_defs"
              auto-save-default               t
              blink-matching-paren            t
              delete-active-region            t
              delete-by-moving-to-trash       t
              disabled-command-function       nil         ;; don't prompt for some disabled functions
              enable-local-variables          :all
              fill-column                     100
              ffap-machine-p-known            'reject     ;; stop attempts at pinging websites on autocomplete
              indent-tabs-mode                nil
              indicate-empty-lines            nil
              inhibit-startup-message         t
              kill-do-not-save-duplicates     t
              linum-format                    " %4d "
              major-mode                      'text-mode
              mode-require-final-newline      t
              next-line-add-newlines          t           ;; adds newline for C-n at end of buffer
              require-final-newline           t
              ring-bell-function              'ignore
              scroll-preserve-screen-position t
              sentence-end-double-space       nil
              show-trailing-whitespace        t
              tab-always-indent               'complete   ;; smart tab behavior - indent or complete
              tab-width                       4
              truncate-lines                  t
              vc-follow-symlinks              t
              visible-bell                    t
              x-stretch-cursor                t           ;; stretch cursor for tab characters.
              )


;; some keybindings

;; auto-indent on Enter
(bind-key "RET" #'newline-and-indent global-map)

;; try and have a normal way to delete things
(bind-key "<delete>" #'delete-region global-map)

;; set an extra command to jump to other window, for convenience
(bind-key "M-o" #'other-window global-map)

;; set a general key for goto-line
(bind-key "C-c l" #'goto-line)


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


;; taken from http://pages.sachachua.com/.emacs.d/Sacha.html
(defun sachachua/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'sachachua/smarter-move-beginning-of-line)


;; disabled for now, using ivy/counsel/swiper
;; setup helm for as many things as possible
(use-package helm
  :disabled t
  :ensure helm
  :diminish helm-mode
  :init
  (require 'helm-config)

  (setq helm-split-window-in-side-p               t
        helm-M-x-fuzzy-match                      t
        helm-move-to-line-cycle-in-source         t
        helm-ff-search-library-in-sexp            t
        helm-ff-file-name-history-use-recentf     t
        helm-ff-skip-boring-files                 t
        helm-autoresize-max-height                40
        helm-autoresize-min-height                40
        helm-use-frame-when-more-than-two-windows nil

        ;; fuzzy matching
        helm-buffers-fuzzy-matching               t
        helm-completion-in-region-fuzzy-match     t
        helm-M-x-fuzzy-match                      t
        helm-apropos-fuzzy-match                  t
        helm-imenu-fuzzy-match                    t
        helm-lisp-fuzzy-completion                t
        helm-locate-fuzzy-match                   t
        helm-mode-fuzzy-match                     t
        helm-recentf-fuzzy-match                  t
        helm-semantic-fuzzy-match                 t
        )

  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))

  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c")) ;; old key is risky

  :bind (("C-x b"   . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)

         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)

         ("C-c h o" . helm-occur)
         ("C-c h x" . helm-register)
         ("C-c h w" . helm-wikipedia-suggest)

         ("C-h f" . helm-apropos)
         ("C-h r" . helm-info-emacs)
         ("C-h C-l" . helm-locate-library)
         ("C-h SPC" . helm-all-mark-rings))

  :config
  (helm-mode)
  (helm-autoresize-mode nil)
  (helm-descbinds-mode))


(use-package helm-projectile
  :disabled t
  :ensure t
  :after helm projectile
  :init
  :custom
  (helm-projectile-fuzzy-match t)
  (projectile-completion-system 'helm)
  :config (helm-projectile-on))


;; use ag for searching in helm
(use-package helm-ag
  :disabled t
  :ensure t
  :after helm
  :commands helm-do-ag
  :bind (("C-c h g" . helm-do-ag)))


;; use GNU global
(use-package helm-gtags
  :disabled t
  :ensure t
  :after helm
  :diminish (helm-gtags-mode . "gtags")
  :bind (:map helm-gtags-mode-map
              ("M-." . helm-gtags-dwim)
              ("C-c C-t c" . helm-gtags-create-tags)
              ("C-c C-t u" . helm-gtags-update-tags)
              ("C-c C-t s" . helm-gtags-select))
  :custom
  (helm-gtags-prefix-key "C-c C-t")
  (helm-gtags-suggested-key-mapping t)
  (helm-gtags-path-style 'relative)
  (helm-gtags-ignore-case t)
  (helm-gtags-auto-update t)
  (helm-gtags--label-option "pygments")
  :init (add-hook 'prog-mode-hook 'helm-gtags-mode))

;; testing out ivy/counsel as replacement for helm
(use-package counsel
  :ensure t
  :demand
  :diminish
  :bind  (("M-x" . counsel-M-x)
          ("M-y" . counsel-yank-pop)
          ("C-x C-f" . counsel-find-file)
          ("C-x C-r" . counsel-recentf)

          ("C-s" . counsel-grep-or-swiper)
          ("C-x l" . counsel-locate)
          ("C-c k" . counsel-rg)
          ("C-c i" . counsel-imenu)

          ("C-h b" . counsel-descbinds)
          ("C-h f" . counsel-apropos)
          ("C-h C-l" . counsel-find-library)
          ("C-h SPC" . counsel-mark-ring))
  :custom
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp "\\.DS_Store\\|.git")
  (counsel-grep-base-command
   "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  :config (counsel-mode 1))

;; provides sorting for ivy
(use-package flx
  :ensure t)

(use-package ivy
  :ensure t
  :demand
  :after flx
  :diminish
  :bind (("C-x C-b" . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map  ;; mimic helm reflexes
         ("C-l" . ivy-backward-delete-char)
         ("C-j" . ivy-alt-done))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-height 10)
  (ivy-wrap t)
  ;; (ivy-initial-inputs-alist nil)
  ;; configure regexp engine
  (ivy-re-builders-alist
   '((t   . ivy--regex-ignore-order)))
  :config (ivy-mode 1))


(use-package counsel-projectile
  :after projectile
  :ensure t
  :config (counsel-projectile-mode))


(use-package counsel-gtags
  :ensure t
  :after counsel
  :diminish (counsel-gtags-mode . "gtags")
  :bind (("M-." . counsel-gtags-dwim)
         ("C-c C-t c" . counsel-gtags-create-tags)
         ("C-c C-t u" . counsel-gtags-update-tags)
         ("C-c C-t d" . counsel-gtags-find-definition)
         ("C-c C-t r" . counsel-gtags-find-reference)
         ("C-c C-t s" . counsel-gtags-find-symbol)
         ("C-c C-t f" . counsel-gtags-go-forward)
         ("C-c C-t b" . counsel-gtags-go-backward)))


;; revert buffers automatically when underlying files are changed externally
(use-package autorevert
  :diminish (auto-revert-mode . "ar")
  :config (global-auto-revert-mode t))


;; linked to key-chords below
(use-package avy
  :ensure t
  :defer t)


;; used in a few places to define keybindings easily
(use-package bind-key)


;; set up some of crux's convenience functions
(use-package crux
  :ensure t
  :bind (("C-c e"   . crux-eval-and-replace)
         ("C-x 4 t" . crux-transpose-windows)
         ("C-c I"   . crux-find-user-init-file)
         ("C-c S"   . crux-find-shell-init-file))
  :config
  ;; kills to end of line first, then whole line
  (global-set-key [remap kill-line] #'crux-smart-kill-line))


;; diminish keeps the modeline tidy
(use-package diminish)


;; define a bunch of quick key combos for basic actions
;; these are a mix of movement and helm calls for convenience
(use-package key-chord
  :ensure t
  :after avy
  :init (key-chord-mode +1)
  :config
  ;; quick avy calls
  (key-chord-define-global "jj" 'avy-goto-word-1)
  (key-chord-define-global "jl" 'avy-goto-line)
  (key-chord-define-global "jk" 'avy-goto-char))


;; save recent files
(use-package recentf
  :init
  (setq recentf-save-file (expand-file-name "recentf" savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))


;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :init
  (setq save-place-file (expand-file-name "saveplace" savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))


;; savehist keeps track of some history
(use-package savehist
  :init
  (setq savehist-additional-variables '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" savefile-dir))
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
  :init (windmove-default-keybindings))


;; unobtrusively trim trailing whitespace
(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config (ws-butler-global-mode))


(provide 'config-ui)
;;; config-ui.el ends here
