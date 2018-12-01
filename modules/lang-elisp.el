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
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
  (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  :config
  (setq mode-name "ELisp")
  ;; keep .elc files updated automatically
  (add-hook 'after-save-hook
            (lambda ()
              (when (and
                     (or (string-prefix-p *modules-dir* (file-truename buffer-file-name))
                         (string-prefix-p *dotfiles-dir* (file-truename buffer-file-name)))
                     (file-exists-p (byte-compile-dest-file buffer-file-name)))
                (emacs-lisp-byte-compile)))
            nil
            t))


(use-package ielm
  :ensure nil
  :config
  (add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook 'smartparens-strict-mode)
  (add-hook 'ielm-mode-hook 'paredit-mode))


(provide 'lang-elisp)
;;; lang-elisp.el ends here
