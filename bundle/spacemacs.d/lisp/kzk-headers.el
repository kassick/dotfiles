;;; kzk-headers.el --- Headers                       -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Rodrigo Kassick

;; Author: Rodrigo Kassick <kassick@antagorda>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun kassick/do-header (leader &optional sleader eleader)
      (let ((com (concat leader " ")))
        (concat (if sleader (concat sleader "\n") "")
                (concat com "File: \"" (buffer-file-name) "\"" "\n")
                (concat com "Created: \"" (current-time-string) "\"" "\n")
                (concat com "Updated: \"" (current-time-string) "\"" "\n" )
                (concat com "$Id$" "\n")
                (concat com "Copyright (C) " (format-time-string "%Y") ", "
                        (user-full-name) "\n")
                (if eleader (concat eleader "\n") ""))))

(defun kassick/c-style-header ()
      (kassick/do-header " *"
                         (concat "/" (make-string 76 ?*))
                         (concat " " (make-string 76 ?*) "/")))

;; Setup auto insert templates
(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/templates/")
(setq auto-insert-query nil)

;; Have to copy here, for autoinsert has no "clear template list for mode"
(setq auto-insert-alist
      '((("\\.\\([Cc]\\|cc\\|cpp\\)\\'" . "C / C++ program")
         nil
         "#include \""
         (let ((stem (file-name-sans-extension buffer-file-name)))
           (cond ((file-exists-p (concat stem ".h"))
	      (file-name-nondirectory (concat stem ".h")))
	     ((file-exists-p (concat stem ".hh"))
	      (file-name-nondirectory (concat stem ".hh")))))
         & ?\" | -10)

        (html-mode . (lambda () (sgml-tag "html")))

        (plain-tex-mode . "tex-insert.tex")
        (bibtex-mode . "tex-insert.tex")
        (latex-mode
         ;; should try to offer completing read for these
         "options, RET: "
         "\\documentclass[" str & ?\] | -1
         ?{ (read-string "class: ") "}\n"
         ("package, %s: "
          "\\usepackage[" (read-string "options, RET: ") & ?\] | -1 ?{ str "}\n")
         _ "\n\\begin{document}\n" _
         "\n\\end{document}")

        (("/bin/.*[^/]\\'" . "Shell-Script mode magic number") .
         (lambda ()
           (if (eq major-mode (default-value 'major-mode))
	   (sh-mode))))

        (("\\.el\\'" . "Emacs Lisp header")
         "Short description: "
         ";;; " (file-name-nondirectory (buffer-file-name)) " --- " str
         (make-string (max 2 (- 80 (current-column) 27)) ?\s)
         "-*- lexical-binding: t; -*-" '(setq lexical-binding t)
         "

;; Copyright (C) " (format-time-string "%Y") "  "
 (getenv "ORGANIZATION") | (progn user-full-name) "

;; Author: " (user-full-name)
'(if (search-backward "&" (line-beginning-position) t)
         (replace-match (capitalize (user-login-name)) t t))
'(end-of-line 1) " <" (progn user-mail-address) ">
;; Keywords: "
 '(require 'finder)
 ;;'(setq v1 (apply 'vector (mapcar 'car finder-known-keywords)))
 '(setq v1 (mapcar (lambda (x) (list (symbol-name (car x))))
		   finder-known-keywords)
	v2 (mapconcat (lambda (x) (format "%12s:  %s" (car x) (cdr x)))
	   finder-known-keywords
	   "\n"))
 ((let ((minibuffer-help-form v2))
        (completing-read "Keyword, C-h: " v1 nil t))
        str ", ") & -2 "

\;; This program is free software; you can redistribute it and/or modify
\;; it under the terms of the GNU General Public License as published by
\;; the Free Software Foundation, either version 3 of the License, or
\;; (at your option) any later version.

\;; This program is distributed in the hope that it will be useful,
\;; but WITHOUT ANY WARRANTY; without even the implied warranty of
\;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\;; GNU General Public License for more details.

\;; You should have received a copy of the GNU General Public License
\;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

\;;; Commentary:

\;; " _ "

\;;; Code:



\(provide '"
           (file-name-base)
           ")
\;;; " (file-name-nondirectory (buffer-file-name)) " ends here\n")
        )
      )
(define-auto-insert "\.org$" "org-header.org")
;(define-auto-insert "\.py" "python-header")


(define-auto-insert
         '(python-mode . "Python Skeleton")
         '(nil
           "#!/usr/bin/env python\n# -*- coding: utf-8 -*-" \n
           \n
           (kassick/do-header "#") \n
           ))
(define-auto-insert
      '("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header")
      '(nil
        (kassick/c-style-header) \n
        \n
        "#pragma once" \n
        \n
        ))
(define-auto-insert
      '("\\.\\([Cc]\\|cc\\|cpp\\)\\'" . "C / C++ program")
      '(nil
        (kassick/c-style-header)
        \n
        )
      )

(add-hook 'before-save-hook (lambda ()
                                  (let ((time-stamp-start "Time-[sS]tamp:[ \t]+\\\\?[\"<]+")) (time-stamp))
                                  (let ((time-stamp-start "Updated:[ \t]+\\\\?[\"<]+")) (time-stamp))
                                  ))



(provide 'kzk-headers)
;;; kzk-headers.el ends here
