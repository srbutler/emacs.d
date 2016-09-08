;;; phoenix-grammar-mode.el --- Summary
;;
;;; Commentary:
;;
;; This simple mode is for editing Phoenix/JPhoenix grammar files. It
;; doesn't provide much more than a few hooks and comment handling for now.
;;
;;; Code:

;; (defvar phoenix-grammar-macros "\([A-Z]*$\|([A-Z]*)\|[	
;;  ][A-Z]*[	
;;  $]\)")

(defvar phoenix-grammar-non-terminals "\[\([_[:alnum:]]*\)]")

(defvar phoenix-grammar-features-parameters
  "\[\(\([_[:alnum:]]*:\)?[_[:alnum:]]*\+\)]")

(defvar phoenix-grammar-values "_[_[:alnum:]]*")

(defvar phoenix-grammar-syntax "[\]\[:*+();]")

(defvar phoenix-grammar-tags "")

(defconst phoenix-grammar-font-lock-keywords
  `(
    ;; (,phoenix-grammar-macros . font-lock-function-name-face)
    (,phoenix-grammar-non-terminals 1 font-lock-keyword-face)
    (,phoenix-grammar-features-parameters 1 font-lock-builtin-face)
    (,phoenix-grammar-values . font-lock-variable-name-face)
    (,phoenix-grammar-syntax . font-lock-constant-face)
    )
  )

(defun phoenix-grammar-comment-dwim (arg)
  (interactive "*P")
  (require 'newcomment)
  (let ((deactivate-mark nil) (comment-start "#") (comment-end ""))
    (comment-dwim arg)))

;;;###autoload
(define-derived-mode phoenix-grammar-mode prog-mode
  "PhoenixGra"
  "Major mode for Phoenix/JPhoenix parser grammars"
  
  ;; enable the basic syntax highlighting
  (setq font-lock-defaults '((phoenix-grammar-font-lock-keywords)))

  ;; comment handling
  (define-key phoenix-grammar-mode-map [remap comment-dwim] 'phoenix-grammar-comment-dwim)
  (modify-syntax-entry ?# "< b" phoenix-grammar-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" phoenix-grammar-mode-syntax-table)
  (setq-local comment-start "# ")
  (setq-local comment-end "")

  ;; autofilling
  (auto-fill-mode 1)

  ;; turn eldoc mode
  (eldoc-mode nil)
  )

;;;###autoload
(setq auto-mode-alist
      (append '(("\\.gra\\'" . phoenix-grammar-mode)) auto-mode-alist))

(provide 'phoenix-grammar-mode)
;;; phoenix-grammar-mode.el ends here
