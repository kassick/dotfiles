;;; kzk-org-capture.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Rodrigo Kassick

;; Author: Rodrigo Kassick <rodrigokassick@mckzk>

(require 'kzk-org-common)

(setq kzk/org-capture-template-context-entry (concat "* %?" "\n"
                                                     ":PROPERTIES:" "\n"
                                                     ":ENTERED:  %U" "\n"
                                                     ":LOCATION:  %a" "\n"
                                                     ":END:" "\n"
                                                     "\n"
                                                     "%i" "\n")
      kzk/org-capture-template-entry (concat "* %?" "\n"
                                             ":PROPERTIES:" "\n"
                                             ":ENTERED:  %U" "\n"
                                             ":END:" "\n"
                                             "\n"
                                             "%i" "\n")
      kzk/org-capture-task-template (concat "* TODO %?" "\n"
                                            ":LOGBOOK:" "\n"
                                            "- State \"TODO\"   from   %U" "\n"
                                            ":END:" "\n"
                                            "\n"
                                            "%i" "\n"))

;; Default org-capture templates, if not set via customize:
(defvar org-capture-templates
  `(("t" "Todo"
     entry
     (file+headline ,(kzk/org-directory-file "todo.org")
                    "Tasks")
     ,kzk/org-capture-task-template :prepend t)
    ("j" "Journal entry with context"
     entry
     (file+datetree ,(kzk/org-directory-file "journal.org"))
     ,kzk/org-capture-template-context-entry)
    ("e" "Journal entry"
     entry
     (file+datetree ,(kzk/org-directory-file "journal.org"))
     ,kzk/org-capture-template-entry)
    ("i" "Item on journal entry"
     item
     (file+function ,(kzk/org-directory-file "journal.org")
                    spacemacs/helm-jump-in-buffer)
     "%?\n%i")))

(defun kzk/add-current-file-to-org-capture (name key header)
  "Adds the current file to the org-capture-templates"
  (interactive "sName: \nsKey: \nsCapture under header: ")

  (require 'org-capture)

  (if (member key
              (mapcar (lambda (e) (car e)) org-capture-templates))
      (message "Key `'%s' is already in use" key)
    (let* ((full-file-name (buffer-file-name))
           (entry `(,key
                    ,name
                    entry
                    (file+olp+datetree ,full-file-name ,header)
                    ,kzk/org-capture-template-entry))
           (new-templates (add-to-list 'org-capture-templates entry t)))
      (customize-save-variable 'org-capture-templates new-templates))))

(defun kzk/org-capture ()
  "Capture a note in org-mode"
  (interactive)
  (require 'org-capture) ;; force load org-capture because helm-org-capture does not requires org-capture :/
  (helm-org-capture-templates))

(with-eval-after-load 'general
  (general-define-key :keymaps 'global
                      "C-c C" 'kzk/org-capture)
  (general-define-key :keymaps 'org-mode-map
                      "C-c A c" '(kzk/add-current-file-to-org-capture :which-key: "adds current file to org-capture-templates")))

(provide 'kzk-org-capture)
