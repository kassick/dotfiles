(defun xdg-open-fun (fname)
  (save-window-excursion
    (message "Running xdg-open for %s" fname)
    (let ((process-connection-type nil))
      (start-process-shell-command (format "/usr/bin/xdg-open '%s'" fname)
                                   nil
                                   (format "/usr/bin/xdg-open '%s'" fname))))
  )

;;; {{{ Finds the agenda files and returns a flat list
(require 'nifty)
(defun org-find-agenda-files ()
  (setq tmplist '())
  (mapcar (lambda (d)
            (setq tmplist (append tmplist
                                  (sa-find-org-file-recursively d "org" 5))))
          org-extra-agenda-files)
  tmplist
  )
;;; }}}

;;; {{{ org-update-agenda-files -- Updates the agenda files
(defun org-update-agenda-files ()
  (interactive)
  (setq org-agenda-files  (org-find-agenda-files))
  (message "Agenda file list updated")
  )
;;; }}}

(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
                       two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y-%m-%d")
                 ((equal prefix '(4)) "[%Y-%m-%d]"))))
    (insert (format-time-string format))))


(defun insert-time-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
                       two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "[%H:%M:%S; %d.%m.%Y]")
                 ((equal prefix '(4)) "[%H:%M:%S; %Y-%m-%d]"))))
    (insert (format-time-string format))))
(defun ndk/checkbox-list-complete ()
  (save-excursion
    (org-back-to-heading t)
    (let ((beg (point)) end)
      (end-of-line)
      (setq end (point))
      (goto-char beg)
      (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
          (if (match-end 1)
              (if (equal (match-string 1) "100%")
                  ;; all done - do the state change
                  (org-todo 'done)
                (org-todo 'todo))
            (if (and (> (match-end 2) (match-beginning 2))
                     (equal (match-string 2) (match-string 3)))
                (org-todo 'done)
              (org-todo 'todo)))))))


(with-eval-after-load 'org
  (with-eval-after-load 'general
   (general-define-key :keymaps 'org-mode-map
                       "C-c d" 'insert-date
                       "C-c t" 'insert-time-date
                       "C-c v" 'org-show-todo-tree)
   )

  (setq org-startup-truncated nil
        ;; revert-without-query (quote ("google.org"))
        org-id-method (quote uuidgen)
                                        ;(setq org-latex-listings 'listings)
        org-hide-emphasis-markers t ;; to hide the *,=, or / markers
        org-pretty-entities t       ;; to have \alpha, \to and others display as utf8 http://orgmode.org/manual/Special-symbols.html
        org-hide-leading-stars t
        org-alphabetical-lists t
        org-log-into-drawer t
        org-src-fontify-natively t  ;; you want this to activate coloring in blocks
        org-src-tab-acts-natively t ;; you want this to have completion in blocks
        org-src-preserve-indentation nil
        org-edit-src-content-indentation 0
        org-latex-pdf-process '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                "%latex -shell-escape -interaction nonstopmode -output-directory %o %f")
        org-latex-compiler "xelatex"
        org-latex-listings 'minted
        org-directory "~/Dropbox/org/"
        org-default-notes-file (concat org-directory "notes.org")
        org-agenda-include-all-todo t
        org-agenda-include-diary t
        org-extra-agenda-files (cl-remove-if-not 'file-exists-p
                                                 `( ,org-directory
                                                    "~/Work/Thesis/"))
        org-agenda-files (org-find-agenda-files)
        org-capture-templates '(("t" "Todo" entry (file+headline
                                                   (concat org-directory "todo.org")
                                                   "Tasks")
                                 "* TODO %?
                                 %i
                                 %a" :prepend t)
                                ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
                                 "* %?
                                 Entered on %U
                                 %i
                                 %a"))
        ;; org-export-babel-evaluate nil
        org-confirm-babel-evaluate nil
        ;;; {{{ Set behaviour for links
        org-file-apps (quote (; (directory . system)   ; uncomment to open directories with xdg-open always
                              (t . emacs)              ; set to system to have xdg-open by default
                              (auto-mode . emacs)
                              ;;; (system . "/usr/bin/xdg-open \"%s\" </dev/null")
                              (system . (lambda (file link) (xdg-open-fun file)))
                              ("\\.mm\\'" . default)
                              ("\\.x?html?\\'" . default)
                              ("\\.pdf\\'" . default))))
  ;; }}}


  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (add-hook 'org-mode-hook 'org-display-inline-images)
  (add-hook 'org-mode-hook 'org-babel-result-hide-all)

  (add-to-list 'org-structure-template-alist
               '("r" "#+BEGIN_SRC R :results output :session *R* :exports both\n\n#+END_SRC" "<src lang=\"R\">\n\n</src>"))

  (add-to-list 'org-structure-template-alist
               '("R" "#+BEGIN_SRC R :results output graphics :file (org-babel-temp-file \"figure\" \".png\") :exports both :width 600 :height 400 :session *R* \n\n#+END_SRC" "<src lang=\"R\">\n\n</src>"))

  (add-to-list 'org-structure-template-alist
               '("RR" "#+BEGIN_SRC R :results output graphics :file  (org-babel-temp-file (concat (file-name-directory (or load-file-name buffer-file-name)) \"figure-\") \".png\") :exports both :width 600 :height 400 :session *R* \n\n#+END_SRC" "<src lang=\"R\">\n\n</src>"))

  (add-to-list 'org-structure-template-alist
               '("p" "#+BEGIN_SRC python :preamble \"# -*- coding: utf-8 -*-\" :results output :exports both\n\n#+END_SRC" "<src lang=\"python\">\n\n</src>"))

  (add-to-list 'org-structure-template-alist
               '("P" "#+BEGIN_SRC python :preamble \"# -*- coding: utf-8 -*-\" :results output :session *python* :exports both\n\n#+END_SRC" "<src lang=\"python\">\n\n</src>"))

  ;; Matplotlib with python!
  (add-to-list 'org-structure-template-alist
               '("MP" "#+BEGIN_SRC python :preamble \"# -*- coding: utf-8 -*-\" :results output graphics :file (org-babel-temp-file \"figure\" \".png\") :exports both :width 600 :height 400\nimport matplotlib, numpy, StringIO\nmatplotlib.use('Agg')\nimport matplotlib.pyplot as plt\nfig=plt.figure(figsize=(4,2))\n\nbuf = StringIO.StringIO()\nplt.savefig(buf)\nprint buf.getvalue()\n#+END_SRC" "<src lang=\"python\">\n\n</src>"))

  (add-to-list 'org-structure-template-alist
               '("Mp" "#+BEGIN_SRC python :preamble \"# -*- coding: utf-8 -*-\" :results output graphics :file \"images/img_?.png\" :exports both :width 600 :height 400\nimport matplotlib, numpy, StringIO\nmatplotlib.use('Agg')\nimport matplotlib.pyplot as plt\nfig=plt.figure(figsize=(4,2))\n\nbuf = StringIO.StringIO()\nplt.savefig(buf)\nprint buf.getvalue()\n#+END_SRC" "<src lang=\"python\">\n\n</src>"))

  (add-to-list 'org-structure-template-alist
               '("b" "#+BEGIN_SRC sh :results output :exports both\n\n#+END_SRC" "<src lang=\"sh\">\n\n</src>"))

  (add-to-list 'org-structure-template-alist
               '("B" "#+BEGIN_SRC sh :session foo :results output :exports both \n\n#+END_SRC" "<src lang=\"sh\">\n\n</src>"))

  (add-to-list 'org-structure-template-alist
               '("g" "#+BEGIN_SRC dot :results output graphics :file \"/tmp/graph.pdf\" :exports both
                       digraph G {
                          node [color=black,fillcolor=white,shape=rectangle,style=filled,fontname=\"Helvetica\"];
                          A[label=\"A\"]
                          B[label=\"B\"]
                          A->B
                       }\n#+END_SRC" "<src lang=\"dot\">\n\n</src>"))

  ;; LaTeX settings
  (with-eval-after-load 'ox-latex
    ;; (message "Loading custom settings for tex")
    (unless (boundp 'org-latex-default-packages-alist)
      (setq org-latex-default-packages-alist nil))
    (add-to-list 'org-latex-default-packages-alist '("" "ifxetex" t))
    (add-to-list 'org-latex-default-packages-alist '("" "minted" t))
    )

;;; Ditaa does not exechute as a jar nowadays, geesh
;;; Have to redefine the function 'cause babel wants to pass
;;; a hell lot of inexistent parameters and then the command
;;; line has a lot of stray arguments that ditaa has no idea
;;; how to handle ...
  (with-eval-after-load 'ob-ditaa
    ;; Dita settings
    (setq org-babel-ditaa-java-cmd "/usr/bin/ditaa"
          org-ditaa-eps-jar-path "/"
          org-ditaa-jar-option "-jar"
          org-ditaa-jar-path "/"
          org-babel-default-header-args:ditaa '((:results . "file")
                                                (:exports . "results")
                                                (:java . "-e UTF-8"))
          )
    (defun org-babel-execute:ditaa (body params)
      "Execute a block of Ditaa code with org-babel.
                        This function is called by `org-babel-execute-src-block'."
      (let* ((result-params (split-string (or (cdr (assoc :results params)) "")))
             (out-file (let ((el (cdr (assoc :file params))))
                         (or el
                             (error
                              "ditaa code block requires :file header argument"))))
             (cmdline (cdr (assoc :cmdline params)))
             (java (cdr (assoc :java params)))
             (in-file (org-babel-temp-file "ditaa-"))
             (eps (cdr (assoc :eps params)))
             (eps-file (when eps
                         (org-babel-process-file-name (concat in-file ".eps"))))
             (pdf-cmd (when (and (or (string= (file-name-extension out-file) "pdf")
                                     (cdr (assoc :pdf params))))
                        (concat
                         "epstopdf"
                         " " eps-file
                         " -o=" (org-babel-process-file-name out-file))))
             (cmd (concat org-babel-ditaa-java-cmd
                          " " java
                          " " cmdline
                          " " (org-babel-process-file-name in-file)
                          " " (if pdf-cmd
                                  eps-file
                                (org-babel-process-file-name out-file)))))
        (unless (file-exists-p org-ditaa-jar-path)
          (error "Could not find ditaa.jar at %s" org-ditaa-jar-path))
        (with-temp-file in-file (insert body))
        (message "Executando comando %S" cmd)
        (shell-command cmd)
        (when pdf-cmd (message pdf-cmd) (shell-command pdf-cmd))
        nil))
  )

  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (latex . t)
     (shell . t)
     (python . t)
     (ditaa . t)
     (dot . t)
     (sqlite . t)
     (perl . t)
     (screen . t)
     (org . t)
     (makefile . t)
     ))

  ;;


  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  (require 'org-protocol)
)

(with-eval-after-load 'org-ref
  (setq org-ref-default-bibliography '("~/Dropbox/Documentos/Bibliografias/IO.bib")
        org-ref-pdf-directory "~/Dropbox/Bibliography/Papers/"
        org-ref-bibliography-notes "~/Dropbox/Bibliography/Papers/notes.org")
)

(provide 'kzk-org)
