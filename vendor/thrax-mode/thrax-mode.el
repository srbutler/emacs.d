;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; Copyright 2005-2011 Google, Inc.
;; Author: rws@google.com (Richard Sproat)

;; Definition of Thrax mode for Emacs.
;; Assuming you have installed this in /usr/local/share/thrax/utils then put the
;; following line in your .emacs:

;; (load "/usr/local/share/thrax/utils/thrax-mode.el")

;; define font-lock keyword sets
(defvar thrax-keywords (regexp-opt '("as" "import") 'words))

(defvar thrax-parse-keywords (regexp-opt '("byte" "utf8") 'words))

(defvar thrax-included-keywords (regexp-opt '("export" "func" "contained") 'words))

(defvar thrax-built-in-functions
  (regexp-opt
   '("Analyzer" "ArcSort" "AssertEmpty" "AssertEqual" "AssertNull"
     "Category" "CDRewrite" "Closure" "Compose" "Concat" "Connect"
     "Determinize" "Difference" "Expand" "Feature" "FeatureVector" "Invert" "LoadFst"
     "LoadFstFromFar" "Minimize" "MPdtCompose" "Optimize" "ParadigmReplace" "PdtCompose"
     "Project" "Replace" "Reverse" "Rewrite" "RmEpsilon" "RmWeight" "StringFile"
     "StringFst" "SymbolTable" "Tagger" "Union")
   'words))

(defvar thrax-syntax "[\]\[=@:|*+\?\"(),;{}-]")
;; (defvar thrax-syntax "[=@:|*+\?,;-]")

(defvar thrax-single-quoted-string "'[^']*'")

(defvar thrax-weight "<[^<>]*>")

(defvar thrax-range "[0-9]+,[0-9]+")

;; Every string not including spaces that is followed by optional
;; spaces and an equals sign is a user-defined fst
(defvar thrax-defined-fst "\\([^ \n]+\\)[ \n]*=")

(defconst thrax-font-lock-keywords
  `((,thrax-defined-fst 1 font-lock-keyword-face)
    ;; (,thrax-defined-fst . 1)  ;; Assign  \1 match to font-lock-keyword-face
    (,thrax-weight . 'font-lock-constant-face)
    (,thrax-single-quoted-string . 'font-lock-string-face)
    (,thrax-range . 'font-lock-constant-face)
    (,thrax-keywords . 'font-lock-preprocessor-face)
    (,thrax-parse-keywords . 'font-lock-preprocessor-face) ;;;
    (,thrax-included-keywords . 'font-lock-preprocessor-face)
    (,thrax-built-in-functions . 'font-lock-function-name-face)
    (,thrax-syntax . 'font-lock-constant-face)
    ))

(defun thrax-comment-dwim (arg)
  (interactive "*P")
  (require 'newcomment)
  (let ((deactivate-mark nil) (comment-start "#") (comment-end ""))
    (comment-dwim arg)))

;;;###autoload
(define-derived-mode thrax-mode fundamental-mode
  "Thrax"
  "Major mode for editing OpenGrm Thrax grammars"
  (setq font-lock-defaults '((thrax-font-lock-keywords)))
  (setq thrax-keywords nil)
  (setq thrax-parse-keywords nil)
  (setq thrax-included-keywords nil)
  (setq thrax-built-in-functions nil)
  (define-key thrax-mode-map [remap comment-dwim] 'thrax-comment-dwim)
  (modify-syntax-entry ?# "< b" thrax-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" thrax-mode-syntax-table)
  ;; Rebind these locally so as not to affect other modes
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  (auto-fill-mode 1)
  ;; make whitespace-cleanup use spaces only
  (setq indent-tabs-mode nil))


(add-hook 'thrax-mode-hook
  (lambda()
    (add-hook 'before-save-hook
      (lambda()
        (save-excursion
        (whitespace-cleanup)))
      nil
      t)))

;;;###autoload
(setq auto-mode-alist
      (append '(("\\.grm\\'" . thrax-mode)) auto-mode-alist))

(provide 'thrax-mode)
;;; thrax-mode.el ends here
