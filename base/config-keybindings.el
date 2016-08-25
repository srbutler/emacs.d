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

(use-package avy
  :ensure t
  :defer t)

(use-package bind-key
  :ensure t)

(use-package key-chord
  :ensure t
  :init (key-chord-mode +1)
  :config
  (key-chord-define-global "jj" 'avy-goto-word-1)
  (key-chord-define-global "jl" 'avy-goto-line)
  (key-chord-define-global "jk" 'avy-goto-char)
  (key-chord-define-global "uu" 'undo-tree-visualize)
  (key-chord-define-global "xx" 'execute-extended-command)
  (key-chord-define-global "yy" 'browse-kill-ring)
  )

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode 1))

(provide 'config-keybindings)
