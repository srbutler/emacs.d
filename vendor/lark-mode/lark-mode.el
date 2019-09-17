;;; lark-mode.el --- Highlight mode for Lark EBNF grammars
;;; Commentary:
;;; Code:

;; ;;;###autoload
;; (define-generic-mode 'ebnf-mode
;;   '(("(*" . "*)"))
;;   '("=")
;;   '(("^[^ \t\n][^=]+" . font-lock-variable-name-face)
;;     ("['\"].*?['\"]" . font-lock-string-face)
;;     ("\\?.*\\?" . font-lock-negation-char-face)
;;     ("\\[\\|\\]\\|{\\|}\\|(\\|)\\||\\|,\\|;" . font-lock-type-face)
;;     ("[^ \t\n]" . font-lock-function-name-face))
;;   '("\\.ebnf\\'")
;;   `(,(lambda () (setq mode-name "EBNF")))
;;   "Major mode for EBNF metasyntax text highlighting.")


;; (font-lock-add-keywords 'c++-mode
;;  `((,(concat
;;    "\\<[_a-zA-Z][_a-zA-Z0-9]*\\>"       ; Object identifier
;;    "\\s *"                              ; Optional white space
;;    "\\(?:\\.\\|->\\)"                   ; Member access
;;    "\\s *"                              ; Optional white space
;;    "\\<\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\>" ; Member identifier
;;    "\\s *"                              ; Optional white space
;;    "(")                                 ; Paren for method invocation
;;    1 'font-lock-function-name-face t)))

(defvar lark-mode-syntax-table
      (let ((synTable (make-syntax-table)))
        ;; C++ style comment “// …”n
        (modify-syntax-entry ?\/ ". 12b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)
        synTable)
      "Syntax table for lark-mode.")


(defvar lark-mode-font-lock-expressions
  '(
    ;; `(,(concat "\\<[_a-zA-Z0-9]*\\>" ":" "\\s *" 1 'font-lock-function-name-face))
    ;; preprocessor directives
    ("%ignore\\|%declare\\|%import" . font-lock-preprocessor-face)
    ;; case-insensitive regexp
    ("(/([^/\\\\\\\\]|\\\\\\\\.)*/)(i)?" . font-lock-regexp-grouping-construct)
    ;; case-insensitive string
    ("(\\\"(([^\\\"\\\\\\\\]|\\\\\\\\.)*)\\\")(i)?". font-lock-string-face)
    ;; quantifier
    ("[*+?|]" . font-lock-negation-char-face)
    ;; or operator
    ("|" . font-lock-negation-char-face)
    ;; alias and assignment operators
    ("->\\|:\\|=" . font-lock-builtin-face)
    ;; terminal
    ("[0-9A-Z_]+" . font-lock-type-face)
    ;; rule
    ("([0-9_a-z]+)" . font-lock-variable-face)
  "Font lock regular expressions."))

;;;###autoload
(define-derived-mode lark-mode prog-mode "Lark"
  "Major mode for editing Lark-extended EBNF grammars"

  (setq font-lock-defaults '(lark-mode-font-lock-expressions))
  (set-syntax-table lark-mode-syntax-table)

  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-end)
  (make-local-variable 'executable-command)

  (setq comment-end ""
        comment-start "//"
        comment-start-skip ".*\\(//.+\\)"
        parse-sexp-ignore-comments t)

  (run-mode-hooks))

(provide 'lark-mode)
;;; lark-mode.el ends here
