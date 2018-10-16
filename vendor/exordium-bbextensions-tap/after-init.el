;; This uses the BbgProtocolHandler set up by the terminal;
;; bbg: links should be supported
(org-add-link-type "bbg" 'org-bbg-open)
(defun org-bbg-open (link)
  "Launch the Bloomberg Terminal with the given function"
  (browse-url (concat "bbg://screens/" link)))

;; Browse docs
(defun browse-doc (arg)
  "Opens the reference doc in eww"
  (interactive "sBrowse what (dmc|dmp|mbs|bmq|bde|mod) ")
  (cond ((string= arg "dmc")
         (eww-open-file
          "/home/uicsbuilder/public_html/dmc/components.html"))
        ((string= arg "dmp")
         (eww-open-file
          "/home/uicsbuilder/public_html/dmp/components.html"))
        ((string= arg "mbs")
         (eww-open-file
          "/home/uicsbuilder/public_html/mbs/components.html"))
        ((string= arg "bmq")
         (eww-open-file
          "/home/uicsbuilder/public_html/bmq/components.html"))
        ((string= arg "mod")
         (eww-open-file
          "/home/modocop/public_html/moda/released/components.html"))
        ((string= arg "bde")
         (eww-browse-url
          "https://bde.bloomberg.com/NonGit/Doxygen/bde_api_prod/"))))

(defun pk/commit-check-promote (force)
  "If promote command exist in the messages, check it's either minor or patch"
  (or force
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "^ *[Pp]romote *: *\\(.*\\)" nil t)
            (or
             (or (string= (match-string 1) "minor")
                 (string= (match-string 1) "patch"))
             (y-or-n-p
              "promote should be either minor or patch.  Commit anyway? "))
          t))))

(add-to-list 'git-commit-finish-query-functions 'pk/commit-check-promote)
