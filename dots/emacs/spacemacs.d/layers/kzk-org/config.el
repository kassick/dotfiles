(defvar org-directory "~/org")

(defvar org-file-apps (quote (
                              ;; (directory . system)   ; uncomment to open directories with xdg-open always
                              (t . emacs)              ; set to system to have xdg-open by default
                              (auto-mode . emacs)
                              ;;; (system . "/usr/bin/xdg-open \"%s\" </dev/null")
                              (system . (lambda (file link) (kzk/open-fun file)))
                              ("\\.mm\\'" . default)
                              ("\\.x?html?\\'" . default)
                              ("\\.pdf\\'" . default))))


(defvar kzk/org-default-notes-file (kzk/org-directory-file "notes.org"))


(custom-set-variables
 '(org-startup-truncated nil)
 '(org-startup-indented t)
 ;; revert-without-query (quote ("google.org"))
 '(org-id-method (quote uuidgen))
 '(org-pretty-entities t)       ;; to have \alpha, \to and others display as utf8 http://orgmode.org/manual/Special-symbols.html
 '(org-hide-leading-stars t)
 ;; allow ordered lists to use a. a) etc. as items.
 '(org-list-allow-alphabetical t)
 '(org-log-into-drawer t)
 ;; you want this to activate coloring in blocks
 '(org-src-fontify-natively t)
 ;; you want this to have completion in blocks
 '(org-src-tab-acts-natively t)
 '(org-src-preserve-indentation nil)
 '(org-edit-src-content-indentation 0)
 '(org-latex-pdf-process '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
                         "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
                         "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"))
 '(org-latex-compiler "xelatex")
 '(org-latex-listings 'minted)
 `(org-default-notes-file ,kzk/org-default-notes-file)
 '(org-agenda-include-all-todo t)
 '(org-agenda-include-diary t)
 `(org-agenda-files (quote ,(list org-directory)))
 '(org-agenda-files '("~/org"))
 '(org-confirm-babel-evaluate nil)
 '(org-capture-bookmark nil)
 )

(setq kzk/org-capture-template-context-entry (concat "* %(kzk/iso-date) %?" "\n"
                                                     ":PROPERTIES:" "\n"
                                                     ":LOCATION:  %a" "\n"
                                                     ":END:" "\n"
                                                     "\n"
                                                     "%i" "\n")
      kzk/org-capture-template-entry (concat "* %(kzk/iso-date) %?" "\n"
                                             "\n"
                                             "%i" "\n")
      kzk/org-capture-task-template (concat "* TODO %?" "\n"
                                            ":LOGBOOK:" "\n"
                                            "- State \"TODO\"   from   %U" "\n"
                                            ":END:" "\n"
                                            "\n"
                                            "%i" "\n"))

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
