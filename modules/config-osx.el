;;; config-osx.el --- Summary:
;;
;;; Commentary: Stuff only to be run on OSX
;;
;;; Code:


;; get the PATH variable working correctly
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :init (exec-path-from-shell-initialize))


;; only run the following on Macs
(when (eq window-system 'mac)

  ;; define key-commands that are common in text-editor interfaces
  ;; a customer version of CUA really
  (global-set-key [(hyper a)] 'mark-whole-buffer)      ;; select all
  (global-set-key [(hyper c)] 'kill-ring-save)         ;; copy
  (global-set-key [(hyper f)] 'counsel-grep-or-swiper) ;; find
  (global-set-key [(hyper l)] 'goto-line)              ;; goto a line
  (global-set-key [(hyper o)] 'helm-find-files)        ;; open a file
  (global-set-key [(hyper r)] 'replace-regexp)         ;; find-and-replace
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


(provide 'config-osx)
;;; config-osx.el ends here
