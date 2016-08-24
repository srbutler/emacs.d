
(defun recompile-elc-on-save ()
  "Recompile your elc when saving an elisp file."
  (add-hook 'after-save-hook
            (lambda ()
              (when (and
                     (or (string-prefix-p modules-dir (file-truename buffer-file-name))
                         (string-prefix-p dotfiles-dir (file-truename buffer-file-name)))
                     (file-exists-p (byte-compile-dest-file buffer-file-name)))
                (emacs-lisp-byte-compile)))
            nil
            t))


(defun srb-emacs-lisp-mode-defaults ()
  "Sensible defaults for `emacs-lisp-mode'."
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1)
  (eldoc-mode +1)
  (recompile-elc-on-save)
  ;; (rainbow-mode +1)
  (setq mode-name "Elisp")
  )

(setq srb-emacs-lisp-mode-hook 'srb-emacs-lisp-mode-defaults)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (run-hooks 'srb-emacs-lisp-mode-hook)))

(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))
