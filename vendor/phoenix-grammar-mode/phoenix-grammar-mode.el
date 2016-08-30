;;; phoenix-grammar-mode.el --- Summary
;;
;;; Commentary:
;;
;; This simple mode is for editing Phoenix/JPhoenix grammar files. It
;; doesn't provide much more than a few hooks and comment handling for now.
;;
;;; Code:

(setq phoenix-highlights
      '(
        ;; ("#.+" . font-lock-comment-face)

        ("^#.*$" . font-lock-comment-face)
        ;; macro definition start
        ("^[A-Z]*$" . font-lock-function-name-face)

        ;; macro call
        ("    (\([A-Z]+\))" . font-lock-function-name-face)
        
        ("\[([A-Z][[:alnum:]]+\+)]" . font-lock-keywords)
        ("\".+\"" . font-lock-string-face)
        
        ))

(define-derived-mode phoenix-grammar-mode prog-mode "PhoenixGra"

  ;; set the mode name
  (setq mode-name "PhoenixGra")
  
  ;; enable the basic syntax highlighting
  (setq font-lock-defaults '(phoenix-highlights))

  ;; comment handling
  (setq-local comment-start "# ")
  (setq-local comment-end "")

  ;; turn eldoc mode
  (eldoc-mode nil)
  )

(provide 'phoenix-grammar-mode)
;;; phoenix-grammar-mode.el ends here
