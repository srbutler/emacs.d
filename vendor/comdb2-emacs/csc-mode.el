;;; csc-mode.el --- major mode for Comdb2 CSC2 files -*- coding: utf-8; lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(defvar csc-mode-section-names
  '("constraints" "constants" "ondisk" "schema" "keys" "tag")
  "List of section names in CSC2 data definition language.")

(defvar csc-mode-type-names
  '("intervaldsus" "decimal128" "datetimeus" "intervalym" "intervalds"
    "decimal32" "decimal64" "longlong" "datetime" "u_short"
    "cstring" "pstring" "double" "short" "u_int" "float" "vutf8"
    "byte" "blob" "int")
  "List of type names in CSC2 data definition language.")

(defvar csc-mode-tags-keywords
  '("dbstore" "dbload" "dbpad" "null")
  "List of special keywords for tag section in CSC2 data definition language.")

(defvar csc-mode-keys-keywords
  '("uniqnulls" "<DESCEND>" "datacopy" "<ASCEND>" "dup")
  "List of special keywords for the keys section in CSC2 data definition language.")

(defvar csc-mode-constraints-keywords
  '("on delete cascade" "on update cascade")
  "List of special keywords for the keys section in CSC2 data definition language.")

(defvar csc-font-lock-keywords
  (let ((sections-regexp (regexp-opt csc-mode-section-names 'words))
        (types-regexp (regexp-opt csc-mode-type-names 'words))
        (tags-regexp (regexp-opt csc-mode-tags-keywords 'words))
        (keys-regexp (regexp-opt csc-mode-keys-keywords 'words))
        (constraints-regexp (regexp-opt csc-mode-constraints-keywords 'words)))
    `((,sections-regexp . font-lock-keyword-face)
      (,types-regexp . font-lock-type-face)
      (,tags-regexp . font-lock-builtin-face)
      (,keys-regexp . font-lock-builtin-face)
      (,constraints-regexp . font-lock-builtin-face)
      ;; list keywords before general regexp-based mappings
      ("^[\t ]*[A-Za-z_]+[\t ]+\\([A-Za-z0-9_]+\\)" . font-lock-variable-name-face)))
  "Syntax highlighting in use in csc-mode buffers.")

(defvar csc-mode-syntax-table
      (let ((synTable (make-syntax-table)))
        ;; C++ style comment “// …”
        (modify-syntax-entry ?\/ ". 12b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)
        synTable)
      "Syntax table for CSC-mode.")

(defun csc-completion-at-point ()
  "This is the function to be used for the hook `completion-at-point-functions'."
  (let ((bounds (bounds-of-thing-at-point 'word))
        (mode-keywords (mapcan (lambda (x) (if (listp x) x nil))
                               '(csc-mode-section-names
                                 csc-mode-type-names
                                 csc-mode-tags-keywords
                                 csc-mode-keys-keywords))))
    (when bounds
      (list (car bounds)
            (cdr bounds)
            mode-keywords
            :exclusive 'no
            :company-docsig #'identity))))

(define-derived-mode csc-mode prog-mode "comdb2-CSC2"
  "Major mode for editing Comdb2 CSC2 files."

  (setq font-lock-defaults '(csc-font-lock-keywords))
  (set-syntax-table csc-mode-syntax-table)
  ;; (use-local-map csc-mode-map)

  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-end)
  (make-local-variable 'executable-command)

  (setq comment-end ""
        comment-start "//"
        comment-start-skip ".*\\(//.+\\)"
        parse-sexp-ignore-comments t)

  ;; hook-in completion at point
  (add-hook 'completion-at-point-functions 'csc-completion-at-point nil 'local)

  (run-mode-hooks))


(provide 'csc-mode)
;;; csc-mode.el ends here
