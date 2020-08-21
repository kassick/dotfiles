;;; kzk-org-common.el --- common settings for org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Rodrigo Kassick

;; Author: Rodrigo Kassick <rodrigokassick@mckzk>

(defcustom kzk/org-directory "~/Dropbox/org"
  "Default path for org-mode {todo,journal}.org")

(defun kzk/org-directory-file (name)
  (expand-file-name name kzk/org-directory))


(provide 'kzk-org-common)
;;; kzk-org-common.el ends here
