

;; (use-package helm
;;   :ensure helm
;;   :diminish helm-mode
;;   :init (progn
;;           (require 'helm-config)
;;           (require 'helm-ag)

;;           (setq helm-split-window-in-side-p           t
;;                 helm-buffers-fuzzy-matching           t
;;                 helm-move-to-line-cycle-in-source     t
;;                 helm-ff-search-library-in-sexp        t
;;                 helm-ff-file-name-history-use-recentf t)
          
;;           ;; (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
;;           ;;       helm-input-idle-delay 0.01 ; this actually updates things reeeelatively quickly.
;;           ;;       helm-scroll-amount 4 ; scroll 4 lines other window using M-/M-
;;           ;;       helm-quick-update t ; do not display invisible candidates
;;           ;;       helm-ff-search-library-in-sexp t ; search for library in require' anddeclare-function' sexp.
;;           ;;       helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
;;           ;;       helm-candidate-number-limit 500 ; limit the number of displayed canidates
;;           ;;       helm-ff-file-name-history-use-recentf t
;;           ;;       ;; helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
;;           ;;       helm-buffers-fuzzy-matching t ; fuzzy matching buffer names when non-nil - useful in helm-mini that lists buffers
;;           ;;       helm-recentf-fuzzy-match t
;;           ;;       helm-M-x-requires-pattern nil
;;           ;;       helm-ff-skip-boring-files t
;;           ;;       helm-M-x-fuzzy-match t)

;;           (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
;;           ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;;           ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;;           ;; cannot change helm-command-prefix-key' oncehelm-config' is loaded.
;;           (global-set-key (kbd "C-c h") 'helm-command-prefix)
;;           (global-unset-key (kbd "C-x c"))

;;           (when (executable-find "curl")
;;             (setq helm-net-prefer-curl t)))
  
;;   :bind (("C-x b" . helm-mini)
;;          ("C-h f" . helm-apropos)
;;          ("C-x C-b" . helm-buffers-list)
;;          ("M-x" . helm-M-x)
;;          ("M-y" . helm-show-kill-ring)
;;          ("C-c h C-c w" . helm-wikipedia-suggest)
;;          ("C-c h o" . helm-occur)
;;          ("C-c h x" . helm-register)
;;          ("C-h C-l" . helm-locate-library)
;;          ("C-h SPC" . helm-all-mark-rings)
;;          ("C-h r" . helm-info-emacs)
;;          ("C-x C-f" . helm-find-files)
;;          ("C-x r j" . jump-to-register)
;;          ("C-x C-r" . helm-recentf))

;;   :config (progn
;;             ;; (define-key helm-map (kbd "") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
;;             (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;;             (define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z

;;             (define-key helm-ag-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
;;             (define-key helm-ag-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
;;             (define-key helm-ag-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)
            
;;             (define-key 'help-command (kbd "C-f") 'helm-apropos)
;;             (define-key 'help-command (kbd "r") 'helm-info-emacs)
;;             (define-key 'help-command (kbd "C-l") 'helm-locate-library)
           
;;             (helm-mode)
;;             (helm-autoresize-mode nil))
  
;;   (substitute-key-definition 'find-tag 'helm-etags-select global-map)
;;   (setq projectile-completion-system 'helm)
;;   (helm-descbinds-mode)

;;   ;; enable Helm version of Projectile with replacment commands
;;   (helm-projectile-on))

;; (provide 'config-helm)
