;;; config-keybindings.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

;; only run the following on Macs
(when (eq window-system 'mac)

  ;; define key-commands that are common in text-editor interfaces
  ;; a customer version of CUA really
  (global-set-key [(hyper a)] 'mark-whole-buffer)      ;; select all
  (global-set-key [(hyper c)] 'kill-ring-save)         ;; copy
  (global-set-key [(hyper f)] 'search-forward)         ;; find
  (global-set-key [(hyper F)] 'search-forward-regexp)  ;; find with regexp
  (global-set-key [(hyper l)] 'goto-line)              ;; goto a line
  (global-set-key [(hyper o)] 'helm-find-files)        ;; open a file
  (global-set-key [(hyper r)] 'replace-string)         ;; find-and-replace
  (global-set-key [(hyper R)] 'replace-regexp)         ;; find-and-replace with regexp
  (global-set-key [(hyper s)] 'save-buffer)            ;; save file
  (global-set-key [(hyper v)] 'yank)                   ;; paste
  (global-set-key [(hyper x)] 'kill-region)            ;; cut
  (global-set-key [(hyper z)] 'undo)                   ;; undo

  ;; set up keyboard to have mac-universal keybindings
  ;; meta => alt/control
  ;; hyper => command
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'hyper)

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
        (setq mac-command-modifier 'meta)))))


;; auto-indent on Enter
(bind-key "RET" #'newline-and-indent global-map)

;; try and have a normal way to delete things
(bind-key "<delete>" #'delete-region global-map)

;; set an extra command to jump to other window, for convenience
(bind-key "M-o" #'other-window global-map)


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
  ;; quick avy calls
  (key-chord-define-global "jj" 'avy-goto-word-1)
  (key-chord-define-global "jl" 'avy-goto-line)
  (key-chord-define-global "jk" 'avy-goto-char))

;; cause I forget things
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode 1))

(provide 'config-keybindings)
;;; config-keybindings.el ends here
