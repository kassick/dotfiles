(defun kzk/org-directory-file (name)
  (expand-file-name name org-directory))

(defun kzk/org-fix-org-header-fonts ()
  ;; Fix fonts blargs
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5
                  org-level-6
                  org-level-7
                  org-level-8
                  ))
    (set-face-attribute face nil :weight 'semi-bold :height 1.0)))

(defun kzk/setup-org-babel ()
  (require 'subr-x)
  ;; Redisplay images after block execution
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

  ;; Start with result blocks hiddent
  (add-hook 'org-mode-hook 'org-babel-result-hide-all)

  ;; Babel blocks definitions
  (add-to-list 'org-structure-template-alist
               '("r" . "src R :results output :session *R* :exports both"))

  (add-to-list 'org-structure-template-alist
               `("R" . ,(string-join '("src R"
                                       ":results output graphics"
                                       ":file (org-babel-temp-file \"figure\" \".png\")"
                                       ":exports both"
                                       ":width 600 :height 400"
                                       ":session *R*")
                                     " ")))

  (add-to-list 'org-structure-template-alist
               '("RR" . "src R :results output graphics :file  (org-babel-temp-file (concat (file-name-directory (or load-file-name buffer-file-name)) \"figure-\") \".png\") :exports both :width 600 :height 400 :session *R*"))

  (add-to-list 'org-structure-template-alist
               '("p" . "src python :preamble \"# -*- coding: utf-8 -*-\" :results output :exports both"))

  (add-to-list 'org-structure-template-alist
               '("P" . "src python :preamble \"# -*- coding: utf-8 -*-\" :results output :session *python* :exports both"))

  ;; Matplotlib with python!
  (add-to-list 'org-structure-template-alist
               `("MP" . ,(concat  (string-join '("src python"
                                                 ":preamble \"# -*- coding: utf-8 -*-\""
                                                 ":results output graphics"
                                                 ":file (org-babel-temp-file \"figure\" \".png\")"
                                                 ":exports both"
                                                 ":width 600"
                                                 ":height 400")
                                               " ")
                                  "\n"
                                  (string-join '("import matplotlib, numpy, StringIO"
                                                 "matplotlib.use('Agg')"
                                                 "import matplotlib.pyplot as plt"
                                                 "fig=plt.figure(figsize=(4,2))"
                                                 ""
                                                 "buf = StringIO.StringIO()"
                                                 "plt.savefig(buf)"
                                                 "print buf.getvalue()")
                                               "\n"))))

  (add-to-list 'org-structure-template-alist
               `("Mp" . ,(concat  (string-join '("src python"
                                                 ":preamble \"# -*- coding: utf-8 -*-\""
                                                 ":results output graphics"
                                                 ":file \"images/img_?.png\""
                                                 ":exports both"
                                                 ":width 600"
                                                 ":height 400")
                                               " ")
                                  "\n"
                                  (string-join '("import matplotlib, numpy, StringIO"
                                                 "matplotlib.use('Agg')"
                                                 "import matplotlib.pyplot as plt"
                                                 "fig=plt.figure(figsize=(4,2))"
                                                 ""
                                                 "buf = StringIO.StringIO()"
                                                 "plt.savefig(buf)"
                                                 "print buf.getvalue()")
                                               "\n"))))

  (add-to-list 'org-structure-template-alist
               '("b" . "src sh :results output :exports both"))

  (add-to-list 'org-structure-template-alist
               '("B" . "src sh :session foo :results output :exports both "))

  (add-to-list 'org-structure-template-alist
               `("g" . ,(string-join '("src dot :results output graphics :file \"/tmp/graph.pdf\" :exports both"
                                      "digraph G {"
                                      "    node [color=black,fillcolor=white,shape=rectangle,style=filled,fontname=\"Helvetica\"];"
                                      "    A[label=\"A\"]"
                                      "    B[label=\"B\"]"
                                      "    A->B"
                                      "}")
                                    "\n")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (latex . t)
     (shell . t)
     (python . t)
     (dot . t)
     (sqlite . t)
     (org . t)
     (makefile . t)))
  )


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
