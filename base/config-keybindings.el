;;; KEYBINDINGS

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
;; (global-set-key [(hyper w)]
;;                 (lambda () (interactive) (delete-window)))
(global-set-key [(hyper x)] 'kill-region)
(global-set-key [(hyper z)] 'undo)

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

(use-package bind-key
  :ensure t)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode 1))

(provide 'config-keybindings)
