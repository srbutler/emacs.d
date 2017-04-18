;;; lang-org.el --- Summary:
;;
;;; Commentary:
;;
;;; Code

(require 'cl)

;; Make RefTeX work with Org-Mode
;; use 'C-c (' instead of 'C-c [' because the latter is already
;; defined in orgmode to the add-to-agenda command.
(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c (") 'reftex-citation))


;; make yasnippet work properly with org-mode
(defun yas-org-very-safe-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (and (fboundp 'yas-expand) (yas-expand))))


;; set up org plus contribs for most swag
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :ensure org-plus-contrib
  :config

  ;; enable yasnippet configuration
  (add-hook 'org-mode-hook
            (lambda ()
              (add-to-list 'org-tab-first-hook
                           'yas-org-very-safe-expand)))

  ;; turn on YASnippet
  (add-hook 'org-mode-hook 'yas-minor-mode)

  ;; turn on RefTeX
  (add-hook 'org-mode-hook 'org-mode-reftex-setup)
  (add-hook 'org-mode-hook 'turn-on-reftex)

  ;; code block highlighting
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-export-with-smart-quotes t
        org-confirm-babel-evaluate nil
        org-replace-disputed-keys t
        org-log-done t)

  ;; set the TODO keyword scopes
  (setq org-todo-keywords
        '((sequence "TODO" "DELAYED" "STARTED" "|" "DONE" "CANCELLED")))

  (setq org-todo-keyword-faces
           '(("STARTED" . org-priority)))

  ;; make windmove work in org-mode
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  ;; org-babel code block enabling
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python     . t)
     (haskell    . t)
     (sh         . t)
     ))

  ;; ensure R blocks are called correctly
  (add-to-list 'org-src-lang-modes '("r" . ess-mode))

  ;; enable LaTeX math-mode entry via CDLaTeX
  (use-package cdlatex-mode
    :init (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
    :defer t
    :diminish (org-cdlatex-mode . "ocdl"))

  ;; downloaded from github, allows linguistics examples via linguex
  ;; or gb4e
  (use-package ox-linguistics
    :load-path "~/.emacs.d/vendor/ox-linguistics/lisp"))

;; Fancy bullet rendering.
(use-package org-bullets
  :ensure t
  :defer t
  :init  (add-hook 'org-mode-hook 'org-bullets-mode))

;; set global keys for org-mode access
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


;; mobile-org setup
;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox/Org")
;; Set to the name of the file where new notes will be stored
;; (setq org-mobile-inbox-for-pull "~/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Org")


;; org-export settings
(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

;; bare form, for incorporating in to other documents
(add-to-list 'org-latex-classes
             '("bare"
               ""
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; general notes/homework/etc.
(add-to-list 'org-latex-classes
             '("general"
               "\\documentclass[11pt,letterpaper,notitlepage]{article}\n\\%usepackage{general-org-xelatex}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; for org-mode thesis export
(add-to-list 'org-latex-classes
             '("thesis"
               "\\documentclass[12pt,letterpaper,oneside,notitlepage]{article}\n\\usepackage{thesis-org-xelatex}"
               ("\\section{%s}" . "\\section{%s}")
               ("\\subsection{%s}" . "\\subsection{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection{%s}")
               ("\\paragraph{%s}" . "\\paragraph{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph{%s}")))

;; from kjhealy
(add-to-list 'org-latex-classes
             '("memarticle"
               "\\documentclass[11pt,oneside,article]{memoir}\n\\usepackage{org-preamble-xelatex}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; from kjhealy
(add-to-list 'org-latex-classes
             '("membook"
               "\\documentclass[11pt,oneside]{memoir}\n\\usepackage{org-preamble-xelatex}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

;; LaTeX compilation command. For orgmode docs we just always use
;; xelatex for convenience. You can change it to pdflatex if you like,
;; just remember to make the adjustments to the packages-alist below.
(setq org-latex-pdf-process
      '("latexmk -pdflatex='xelatex -synctex=1 --shell-escape' -pdf %f"))

;; Default packages included in the tex file. As before,
;; org-preamble-xelatex is part of latex-custom-kjh. There's
;; org-preamble-pdflatex as well, if you wish to use that instead.
(setq org-latex-default-packages-alist nil)
(setq org-latex-packages-alist
      '(
        ;; ("" "org-preamble-xelatex" t)    ;; disabled for flexibility
        ;; ("" "graphicx" t)
        ;; ("" "linguex" t)
        ;; ("" "float" )
        ))

;; ebib types for biblatex (from kjhealy)
(org-add-link-type "ebib" 'ebib)
(org-add-link-type
 "cite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "cite:" desc)))
         (format "\\cite{%s}" path)
       (format "\\cite[%s]{%s}" desc path))))))

(org-add-link-type
 "parencite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "parencite:" desc)))
         (format "\\parencite{%s}" path)
       (format "\\parencite[%s]{%s}" desc path))))))

(org-add-link-type
 "textcite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "textcite:" desc)))
         (format "\\textcite{%s}" path)
       (format "\\textcite[%s]{%s}" desc path))))))

(org-add-link-type
 "autocite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "autocite:" desc)))
         (format "\\autocite{%s}" path)
       (format "\\autocite[%s]{%s}" desc path))))))

(org-add-link-type
 "footcite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "footcite:" desc)))
         (format "\\footcite{%s}" path)
       (format "\\footcite[%s]{%s}" desc path))))))

(org-add-link-type
 "fullcite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "fullcite:" desc)))
         (format "\\fullcite{%s}" path)
       (format "\\fullcite[%s]{%s}" desc path))))))

(org-add-link-type
 "citetitle" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "citetitle:" desc)))
         (format "\\citetitle{%s}" path)
       (format "\\citetitle[%s]{%s}" desc path))))))

(org-add-link-type
 "citetitles" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "citetitles:" desc)))
         (format "\\citetitles{%s}" path)
       (format "\\citetitles[%s]{%s}" desc path)
       )))))

(org-add-link-type
 "headlessfullcite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "headlessfullcite:" desc)))
         (format "\\headlessfullcite{%s}" path)
       (format "\\headlessfullcite[%s]{%s}" desc path))))))

;; time manipulation macro for org-tables
(defun org-time-string-to-seconds (s)
  "Convert a string HH:MM:SS to a number of seconds."
  (cond
   ((and (stringp s)
         (string-match "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)" s))
    (let ((hour (string-to-number (match-string 1 s)))
          (min (string-to-number (match-string 2 s)))
          (sec (string-to-number (match-string 3 s))))
      (+ (* hour 3600) (* min 60) sec)))
   ((and (stringp s)
         (string-match "\\([0-9]+\\):\\([0-9]+\\)" s))
    (let ((min (string-to-number (match-string 1 s)))
          (sec (string-to-number (match-string 2 s))))
      (+ (* min 60) sec)))
   ((stringp s) (string-to-number s))
   (t s)))

(defun org-time-seconds-to-string (secs)
  "Convert a number of seconds to a time string."
  (cond ((>= secs 3600) (format-seconds "%h:%.2m:%.2s" secs))
        ((>= secs 60) (format-seconds "%m:%.2s" secs))
        (t (format-seconds "%s" secs))))

(defmacro with-time (time-output-p &rest exprs)
  "Evaluate an org-table formula, converting all fields that look
like time data to integer seconds.  If TIME-OUTPUT-P then return
the result as a time value."
  (list
   (if time-output-p 'org-time-seconds-to-string 'identity)
   (cons 'progn
         (mapcar
          (lambda (expr)
            `,(cons (car expr)
                    (mapcar
                     (lambda (el)
                       (if (listp el)
                           (list 'with-time nil el)
                         (org-time-string-to-seconds el)))
                     (cdr expr))))
          `,@exprs))))

(provide 'lang-org)
;;; lang-org.el ends here
