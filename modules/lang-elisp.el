;;; lang-elisp.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:


;; smartly visit an ielm buffer
(defun visit-ielm ()
  "Create or visit a `ielm' buffer."
  (interactive)
  (if (not (get-buffer "*ielm*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ielm))
    (switch-to-buffer-other-window "*ielm*")))


(use-package emacs-lisp-mode
  :ensure nil
  :mode (("\\.el\\'" . emacs-lisp-mode)
         ("Cask\\'" . emacs-lisp-mode))
  :bind (:map emacs-lisp-mode-map
              ("C-c C-z" . visit-ielm)
              ("C-c C-j" . eval-print-last-sexp)
              :map lisp-interaction-mode-map
              ("C-c C-z" . visit-ielm)
              ("C-c C-j" . eval-print-last-sexp))
  :config
  (defun srb/emacs-lisp-hook ()
    (setq mode-name "ELisp")
    (counsel-gtags-mode -1)
    (rainbow-mode t))
  (add-hook 'emacs-lisp-mode-hook 'srb/emacs-lisp-hook)

  ;; keep .elc files updated automatically
  (defun srb/byte-compile-this-file ()
    (when (and
           (or (string-prefix-p *modules-dir* (file-truename buffer-file-name))
               (string-prefix-p *dotfiles-dir* (file-truename buffer-file-name)))
           (file-exists-p (byte-compile-dest-file buffer-file-name)))
      (emacs-lisp-byte-compile)))
  (add-hook 'after-save-hook 'srb/byte-compile-this-file nil t))


(use-package ielm
  :ensure nil
  :hook ((ielm-mode . rainbow-delimiters-mode)
         (ielm-mode . smartparens-strict-mode)))


(provide 'lang-elisp)
;;; lang-elisp.el ends here
