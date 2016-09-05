;;; config-keybindings.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

;;; KEYBINDINGS

;; try and have a normal way to delete things
(global-set-key (kbd "<delete>") 'delete-region)

;; set up keyboard to have mac-universal keybindings
;; meta => alt/control
;; hypter => command
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)

(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper l)] 'goto-line)
(global-set-key [(hyper x)] 'kill-region)
(global-set-key [(hyper z)] 'undo)
;; (global-set-key [(hyper w)]
;;                 (lambda () (interactive) (delete-window)))

;; this function will switch the command and option bindings between
;; the standard layout and the more mac-centric layout (that will
;; allow for certain special character inputs with alt key)
(defun mac-switch-meta nil
  "Switch meta between Option and Command."
  (interactive)
  (if (eq mac-option-modifier nil)
      (progn
        (setq mac-option-modifier 'meta)
        (setq mac-command-modifier 'hyper))
    
    (progn
      (setq mac-option-modifier nil)
      (setq mac-command-modifier 'meta))))

(if (null window-system)
    (define-key key-translation-map (kbd "C-\\") (kbd "C-;")))

;; the opposite of fill-parapgraph
(defun unfill-paragraph ()
  "Take a multi-line paragraph and make it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

 (define-key global-map "\M-Q" 'unfill-paragraph)


;; linked to key-chords below
(use-package avy
  :ensure t
  :defer t)

;; used in a few places to define keybindings easily
;; I think this a use-package dependency for the :bind command
(use-package bind-key
  :ensure t)

;; define a bunch of quick key combos for basic actions
;; these are a mix of movement and helm calls for convenience
(use-package key-chord
  :ensure t
  :init (key-chord-mode +1)
  :config
  ;; quick helm calls
  (key-chord-define-global "bn" 'helm-buffers-list)
  (key-chord-define-global "df" 'helm-find-files)
  (key-chord-define-global "xx" 'helm-M-x)

  ;; quick avy calls
  (key-chord-define-global "jj" 'avy-goto-word-1)
  (key-chord-define-global "jl" 'avy-goto-line)
  (key-chord-define-global "jk" 'avy-goto-char)
  )

;; cause I forget things
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode 1))

(provide 'config-keybindings)
;;; config-keybindings.el ends here
