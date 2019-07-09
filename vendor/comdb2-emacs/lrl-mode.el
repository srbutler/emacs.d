(provide 'lrl-mode)

(defvar lrl-mode-map (make-sparse-keymap)
  "Local keymap for lrl-mode buffers.")

(defvar lrl-mode-comment-start "#"
  "Comment string to use in lrl-mode buffers.")

(defvar lrl-mode-syntax-table (make-syntax-table)
  "Syntax table in use in lrl-mode buffers.")

(defvar lrl-font-lock-keywords
  '(
    ("^[\t ]*#.*" . font-lock-comment-face)
    ("^[\t ]*\\<\\(name\\|dbnum\\|dir\\)\\>" 1 font-lock-keyword-face)
    ("^[\t ]*\\<\\(table\\|data\\|index\\|cachekb\\|allow\\|queue\\|consumer\\|resource\\|procedure\\|dtastripe\\|nullsort\\)\\>"
     1 font-lock-type-face)
    )
  "Keywords to highlight in LRL mode")

(if lrl-mode-syntax-table ()
  (setq lrl-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_  "w"  lrl-mode-syntax-table)
  )

(defun lrl-mode ()
  "Mode for Bloomberg LRL files"
  (interactive)
  (kill-all-local-variables)
  (use-local-map             lrl-mode-map)
  (set-syntax-table          lrl-mode-syntax-table)

  (make-local-variable       'parse-sexp-ignore-comments)
  (make-local-variable       'comment-start)
  (make-local-variable       'comment-start-skip)
  (make-local-variable       'comment-end)
  (make-local-variable       'executable-command)

  (setq major-mode          'lrl-mode
	mode-name           "LRL"
	comment-end         ""
	comment-start       lrl-mode-comment-start
	comment-start-skip  "[#]+ *"
	parse-sexp-ignore-comments t
	)

  (run-hooks 'lrl-mode-hook)
  )

(if (boundp 'font-lock-defaults-alist)
    (add-to-list
     'font-lock-defaults-alist
     (cons 'lrl-mode
	   (list 'lrl-font-lock-keywords nil t nil nil))))
