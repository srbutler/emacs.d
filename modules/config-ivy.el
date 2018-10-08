;;; config-ivy.el -- Summary
;;
;;; Commentary:
;;    Load this in `init.el' if to use it instead of helm.
;;; Code:

;; adds usage ordering to counsel-M-x
(use-package smex
  :ensure t
  :config (setq smex-save-file
                (expand-file-name ".smex-items" *savefile-dir*)))



;; testing out ivy/counsel as replacement for helm
(use-package counsel
  :ensure t
  :after smex
  :ensure-system-package rg
  :demand
  :diminish
  :bind  (("M-x" . counsel-M-x)
          ("M-y" . counsel-yank-pop)
          ("C-x C-f" . counsel-find-file)
          ("C-x C-r" . counsel-recentf)

          ("C-s" . counsel-grep-or-swiper)
          ("C-r" . counsel-grep-or-swiper)
          ("C-x l" . counsel-locate)
          ("C-c k" . counsel-rg)
          ("C-c i" . counsel-imenu)

          ("C-h b" . counsel-descbinds)
          ("C-h f" . counsel-apropos)
          ("C-h C-l" . counsel-find-library)
          ("C-h SPC" . counsel-mark-ring))
  :custom
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp
   "\\.DS_Store\\|.git\\|\.*~undo-tree~\\|GPATH\\|GRTAGS\\|GTAGS\\|.*.elc")
  (counsel-grep-base-command
   "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (counsel-)
  :config (counsel-mode 1))


;; provides sorting for ivy
(use-package flx
  :ensure t)


(use-package ivy
  :ensure t
  :demand
  :after flx
  :diminish
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x C-b" . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map  ;; mimic helm reflexes
         ("C-l" . ivy-backward-delete-char)
         ("C-j" . ivy-alt-done)
         ("<return>" . ivy-alt-done))
  :custom
  (ivy-initial-inputs-alist nil)  ;; don't start ivy with ^
  (ivy-use-selectable-prompt t)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-height 10)
  (ivy-wrap t)
  ;; configure regexp engine
  (ivy-re-builders-alist
   '((t . ivy--regex-ignore-order)))
  :config (ivy-mode 1))


;; a better version of ivy-switch-buffer
(use-package ivy-rich
  :ensure t
  :after ivy
  :config (ivy-rich-mode 1))


;; hydra presents menus for ivy commands.
(use-package ivy-hydra
  :ensure t
  :after ivy)


;; use ivy for xref candidates
(use-package ivy-xref
  :ensure t
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))


;; project browsing
(use-package counsel-projectile
  :after (counsel projectile)
  :ensure t
  :bind (("C-c p p" . counsel-projectile-switch-project)
         ("C-c p f" . counsel-projectile-find-file)
         ("C-c p g" . counsel-projectile-find-file-dwim)
         ("C-c p d" . counsel-projectile-find-dir)
         ("C-c p b" . counsel-projectile-switch-to-buffer)
         ("C-c p s a" . counsel-projectile-ag)
         ("C-c p s g" . counsel-projectile-grep)
         ("C-c p s i" . counsel-projectile-git-grep)
         ("C-c p s r" . counsel-projectile-rg)
         ("C-c p SPC" . counsel-projectile)
         ("C-c p o" . counsel-projectile-org-capture))
  :config (counsel-projectile-mode))


;; ivy interface for yasnippet
(use-package ivy-yasnippet
  :ensure t
  :after (ivy yasnippet)
  :bind ("C-c y" . ivy-yasnippet))


;; access to GNU Global tags
(use-package counsel-gtags
  :ensure t
  :ensure-system-package global
  :after counsel
  :diminish (counsel-gtags-mode . "gtags")
  :init (add-hook 'prog-mode-hook 'counsel-gtags-mode)
  :bind (:map counsel-gtags-mode-map
         ("M-." . counsel-gtags-dwim)
         ("C-c C-t c" . counsel-gtags-create-tags)
         ("C-c C-t u" . counsel-gtags-update-tags)
         ("C-c C-t d" . counsel-gtags-find-definition)
         ("C-c C-t r" . counsel-gtags-find-reference)
         ("C-c C-t s" . counsel-gtags-find-symbol)
         ("C-c C-t f" . counsel-gtags-go-forward)
         ("C-c C-t b" . counsel-gtags-go-backward)))


;; browse documentation
(use-package counsel-dash
  :ensure t
  :after counsel
  :bind ("C-c d" . counsel-dash)
  :config
  ;; browse in emacs
  (setq counsel-dash-browser-func 'eww-browse-url)

  ;; set hooks for docsets
  ;; TODO: write a function for this
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (setq-local counsel-dash-docsets '("Emacs_Lisp"))))
  (add-hook 'python-mode-hook
            (lambda () (setq-local counsel-dash-docsets
                                   '("Python_3" "Python_2" "NumPy" "SciPy" "Pandas"))))
  (add-hook 'ess-mode-hook
            (lambda () (setq-local counsel-dash-docsets '("R"))))
  (add-hook 'LaTeX-mode-hook
            (lambda () (setq-local counsel-dash-docsets '("LaTeX"))))
  (add-hook 'clojure-mode-hook
            (lambda () (setq-local counsel-dash-docsets '("Clojure"))))
  (add-hook 'java-mode-hook
            (lambda () (setq-local counsel-dash-docsets '("Java_SE8" "Java_SE9"))))
  (add-hook 'lisp-mode-hook
            (lambda () (setq-local counsel-dash-docsets '("Common_Lisp"))))
  (add-hook 'js2-mode-hook
            (lambda () (setq-local counsel-dash-docsets '("Javascript"))))
  (add-hook 'scala-mode-hook
            (lambda () (setq-local counsel-dash-docsets '("Scala"))))
  (add-hook 'haskell-mode-hook
            (lambda () (setq-local counsel-dash-docsets '("Haskell"))))
  (add-hook 'tuareg-mode-hook
            (lambda () (setq-local counsel-dash-docsets '("Ocaml"))))
  (add-hook 'rust-mode-hook
            (lambda () (setq-local counsel-dash-docsets '("Rust"))))
  (add-hook 'go-mode-hook
            (lambda () (setq-local counsel-dash-docsets '("Go"))))
  (add-hook 'c-mode-hook
            (lambda () (setq-local counsel-dash-docsets '("C"))))
  (add-hook 'c++-mode-hook
            (lambda () (setq-local counsel-dash-docsets '("C++")))))


(provide 'config-ivy)
;;; config-ivy.el ends here
