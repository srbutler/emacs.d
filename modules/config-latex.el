;;; package --- Summary:
;; config-lagex.el
;;
;;; Commentary:
;;  Taken from: https://github.com/Schnouki/dotfiles/, with
;;  modifications
;;
;;; Code:

;; Basic settings
(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :config
  (progn
    (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
    (add-hook 'LaTeX-mode-hook #'flyspell-mode)
    (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
    (add-hook 'LaTeX-mode-hook #'turn-on-auto-fill)
    ;;(add-hook 'LaTeX-mode-hook #'(set-fill-column 80))
    (setq TeX-auto-save t
	  TeX-parse-self t
	  TeX-save-query nil
	  TeX-PDF-mode t
          TeX-command-default "xelatexmk"
          TeX-open-quote "“"
          TeX-close-quote "”"
          default-justification 'left
          )
    (setq-default TeX-master nil)

    ;; set up Biber
    (eval-after-load "tex"
      '(add-to-list 'TeX-command-list
                    '("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber") t))

    ;; set up xelatex
    (eval-after-load "tex"
      '(add-to-list 'TeX-command-list
                    '("xelatexmk" "latexmk -synctex=1 -shell-escape -xelatex %s"
                      TeX-run-TeX nil t :help "Process file with xelatexmk")))
    )

  ;; pandoc setup
  (add-hook 'TeX-mode-hook 'pandoc-mode)
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

  )

(use-package reftex
  :commands turn-on-reftex
  :diminish reftex-mode
  :config
  (progn
    (setq reftex-plug-into-AUCTeX t
          reftex-enable-partial-scans t
          reftex-save-parse-info t
          reftex-use-multiple-selection-buffers t
          reftex-cite-prompt-optional-args nil
          reftex-cite-cleanup-optional-args t))

  ;; biblatex commands, not for use with natbib
  (setq reftex-cite-format
        '(
          (?\C-m . "\\cite[]{%l}")
          (?t    . "\\textcite{%l}")
          (?a    . "\\autocite[]{%l}")
          (?p    . "\\parencite{%l}")
          (?f    . "\\footcite[][]{%l}")
          (?F    . "\\fullcite[]{%l}")
          (?P    . "[@%l]")
          (?T    . "@%l [p. ]")
          (?x    . "[]{%l}")
          (?X    . "{%l}")
          ))

  (setq font-latex-match-reference-keywords
        '(
          ("cite" "[{")
          ("cites" "[{}]")
          ("footcite" "[{")
          ("footcites" "[{")
          ("parencite" "[{")
          ("textcite" "[{")
          ("fullcite" "[{")
          ("citetitle" "[{")
          ("citetitles" "[{")
          ("headlessfullcite" "[{")
          )
        )

  ;; Default bibliography
  (setq reftex-default-bibliography
        '("/Users/srbutler/Documents/Bibliography/library.bib"))
  )

(use-package bibtex
  :mode ("\\.bib" . bibtex-mode)
  :init
  (progn
    (setq bibtex-align-at-equal-sign t)
    (add-hook 'bibtex-mode-hook (lambda () (set-fill-column 120)))))


;; Use Biber with AucTeX instead of bibtex
;; called in use-package for TeX above
(defun TeX-run-Biber (name command file)
  "Create a process for NAME using COMMAND to format FILE with Biber."
  (let ((process (TeX-run-command name command file)))
    (setq TeX-sentinel-function 'TeX-Biber-sentinel)
    (if TeX-process-asynchronous
        process
      (TeX-synchronous-sentinel name file process))))

(defun TeX-Biber-sentinel (process name)
  "Cleanup TeX output buffer after running Biber."
  (goto-char (point-max))
  (cond
   ;; Check whether Biber reports any warnings or errors.
   ((re-search-backward (concat
                         "^(There \\(?:was\\|were\\) \\([0-9]+\\) "
                         "\\(warnings?\\|error messages?\\))") nil t)
    ;; Tell the user their number so that she sees whether the
    ;; situation is getting better or worse.
    (message (concat "Biber finished with %s %s. "
                     "Type `%s' to display output.")
             (match-string 1) (match-string 2)
             (substitute-command-keys
              "\\\\[TeX-recenter-output-buffer]")))
   (t
    (message (concat "Biber finished successfully. "
                         "Run LaTeX again to get citations right."))))
  (setq TeX-command-next TeX-command-default))

;; Auto-fill for LaTeX
(defun srbutler/latex-auto-fill ()
  "Turn on auto-fill for LaTeX mode."
  (turn-on-auto-fill)
  (set-fill-column 80)
  ;; (setq default-justification 'left))
(add-hook 'LaTeX-mode-hook #'srbutler/latex-auto-fill))


;;; config-latex.el ends here
