;;; kzk-org-agenda.el --- org agenda helpers         -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Rodrigo Kassick

;; Author: Rodrigo Kassick <rodrigokassick@mckzk>

(require 'kzk-org-common)

(defvar org-agenda-files `(,kzk/org-directory))

(defun kzk/add-current-file-to-org-agenda ()
    "Adds the current file to org agenda list -- and save the list as customize"
    (interactive)

    (require 'org-agenda)

    (let* ((fname (buffer-file-name))
           (agenda-list (if (listp org-agenda-files)
                            org-agenda-files
                          (list org-agenda-files)))
           (new-agenda-list (cons fname agenda-list)))

      (delete-dups new-agenda-list)
      (message "new agenda list %S --- " new-agenda-list )

      (customize-save-variable 'org-agenda-files new-agenda-list)))

(with-eval-after-load 'org
    (with-eval-after-load 'general
        (general-define-key :keymaps 'org-mode-map
                            "C-c A a" '(kzk/add-current-file-to-org-agenda :which-key: "adds current file to org-agenda-files"))))


(provide 'kzk-org-agenda)
;;; kzk-org-agenda.el ends here
