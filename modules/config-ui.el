;;; config-ui.el -- Summary
;;
;;; Commentary:
;;
;;; Code:


;; put my user info
(setq user-full-name "Steven Butler"
      user-mail-address "srbutler@gmail.com")

;; open my work org file
(defun open-kasisto-org ()
  "Open the Kasisto org file."
  (interactive)
  (find-file "~/Dropbox/Kasisto/kasisto.org"))

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
(global-hl-line-mode +1)

;; delete on selection
(add-hook 'prog-mode-hook 'delete-selection-mode)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; hippie expand is dabbrev expand on steroids
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
(setq-default
 auto-save-default               t
 blink-matching-paren            t
 delete-active-region            t
 disabled-command-function       nil         ;; don't prompt for some disabled functions
 enable-local-variables          :all
 indent-tabs-mode                nil
 indicate-empty-lines            nil
 inhibit-startup-message         t
 kill-do-not-save-duplicates     t
 major-mode                      'text-mode
 next-line-add-newlines          t           ;; adds newline for C-n at end of buffer
 require-final-newline           t
 ring-bell-function              'ignore
 scroll-preserve-screen-position t
 show-trailing-whitespace        nil
 tab-always-indent               'complete   ;; smart tab behavior - indent or complete
 tab-width                       4
 truncate-lines                  t
 visible-bell                    t
 x-stretch-cursor                t           ;; stretch cursor for tab characters.
 )


;; setup helm for as many things as possible
(use-package helm
  :ensure helm
  :diminish helm-mode
  :init (progn
          (require 'helm-config)
          (require 'helm-ag)

          (setq helm-split-window-in-side-p           t
                helm-buffers-fuzzy-matching           t
                helm-M-x-fuzzy-match                  t
                helm-recentf-fuzzy-match              t
                helm-move-to-line-cycle-in-source     t
                helm-ff-search-library-in-sexp        t
                helm-ff-file-name-history-use-recentf t
                helm-ff-skip-boring-files             t
                helm-autoresize-max-height            40
                helm-autoresize-min-height            40
                )
          
          (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

          ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
          ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
          ;; cannot change helm-command-prefix-key' oncehelm-config' is loaded.
          (global-set-key (kbd "C-c h") 'helm-command-prefix)
          (global-unset-key (kbd "C-x c"))

          (when (executable-find "curl")
            (setq helm-net-prefer-curl t)))
  
  :bind (("C-x b" . helm-mini)
         ("C-h f" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-c h C-c w" . helm-wikipedia-suggest)
         ("C-c h o" . helm-occur)
         ("C-c h x" . helm-register)
         ("C-h C-l" . helm-locate-library)
         ("C-h SPC" . helm-all-mark-rings)
         ("C-h r" . helm-info-emacs)
         ("C-x C-f" . helm-find-files)
         ("C-x r j" . jump-to-register)
         ("C-x C-r" . helm-recentf))

  :config (progn
            (define-key helm-ag-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
            (define-key helm-ag-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
            (define-key helm-ag-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)
            
            (define-key 'help-command (kbd "C-f") 'helm-apropos)
            (define-key 'help-command (kbd "r") 'helm-info-emacs)
            (define-key 'help-command (kbd "C-l") 'helm-locate-library)
           
            (helm-mode)
            ;; (setq helm-autoresize-max-height 40)
            ;; (setq helm-autoresize-min-height helm-autoresize-max-height)
            (helm-autoresize-mode nil))
  
  (substitute-key-definition 'find-tag 'helm-etags-select global-map)
  (setq projectile-completion-system 'helm)
  (helm-descbinds-mode)

  ;; enable Helm version of Projectile with replacment commands
  (helm-projectile-on))

;; set up some of crux's convenience functions
(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
         ("C-x 4 t" . crux-transpose-windows)
         ("C-c I" . crux-find-user-init-file)
         ("C-c S" . crux-find-shell-init-file)) )


;; get the PATH variable working correctly
(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))


;; diminish keeps the modeline tidy
(require 'diminish)

(use-package idle-highlight-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'idle-highlight-mode))

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
(require 'saveplace)
(setq save-place-file (expand-file-name "saveplace" savefile-dir))
;; activate it for all buffers
(setq-default save-place t)


;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" savefile-dir))
(savehist-mode +1)


;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers


;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)


(provide 'config-ui)
