;;; kzk-org-babel.el --- org-babel specific          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Rodrigo Kassick

;; Author: Rodrigo Kassick <rodrigokassick@mckzk>

;; Redisplay images after block execution
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

;; Start with result blocks hiddent
(add-hook 'org-mode-hook 'org-babel-result-hide-all)

;; Babel blocks definitions
(add-to-list 'org-structure-template-alist
              '("r" . "#+BEGIN_SRC R :results output :session *R* :exports both\n\n#+END_SRC"))

(add-to-list 'org-structure-template-alist
              '("R" . "#+BEGIN_SRC R :results output graphics :file (org-babel-temp-file \"figure\" \".png\") :exports both :width 600 :height 400 :session *R* \n\n#+END_SRC"))

(add-to-list 'org-structure-template-alist
              '("RR" . "#+BEGIN_SRC R :results output graphics :file  (org-babel-temp-file (concat (file-name-directory (or load-file-name buffer-file-name)) \"figure-\") \".png\") :exports both :width 600 :height 400 :session *R* \n\n#+END_SRC"))

(add-to-list 'org-structure-template-alist
              '("p" . "#+BEGIN_SRC python :preamble \"# -*- coding: utf-8 -*-\" :results output :exports both\n\n#+END_SRC"))

(add-to-list 'org-structure-template-alist
              '("P" . "#+BEGIN_SRC python :preamble \"# -*- coding: utf-8 -*-\" :results output :session *python* :exports both\n\n#+END_SRC"))

;; Matplotlib with python!
(add-to-list 'org-structure-template-alist
              '("MP" . "#+BEGIN_SRC python :preamble \"# -*- coding: utf-8 -*-\" :results output graphics :file (org-babel-temp-file \"figure\" \".png\") :exports both :width 600 :height 400\nimport matplotlib, numpy, StringIO\nmatplotlib.use('Agg')\nimport matplotlib.pyplot as plt\nfig=plt.figure(figsize=(4,2))\n\nbuf = StringIO.StringIO()\nplt.savefig(buf)\nprint buf.getvalue()\n#+END_SRC"))

(add-to-list 'org-structure-template-alist
              '("Mp" . "#+BEGIN_SRC python :preamble \"# -*- coding: utf-8 -*-\" :results output graphics :file \"images/img_?.png\" :exports both :width 600 :height 400\nimport matplotlib, numpy, StringIO\nmatplotlib.use('Agg')\nimport matplotlib.pyplot as plt\nfig=plt.figure(figsize=(4,2))\n\nbuf = StringIO.StringIO()\nplt.savefig(buf)\nprint buf.getvalue()\n#+END_SRC"))

(add-to-list 'org-structure-template-alist
              '("b" . "#+BEGIN_SRC sh :results output :exports both\n\n#+END_SRC"))

(add-to-list 'org-structure-template-alist
              '("B" . "#+BEGIN_SRC sh :session foo :results output :exports both \n\n#+END_SRC"))

(add-to-list 'org-structure-template-alist
              '("g" . "#+BEGIN_SRC dot :results output graphics :file \"/tmp/graph.pdf\" :exports both
                      digraph G {
                        node [color=black,fillcolor=white,shape=rectangle,style=filled,fontname=\"Helvetica\"];
                        A[label=\"A\"]
                        B[label=\"B\"]
                        A->B
                      }\n#+END_SRC"))

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
    (makefile . t)))

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
      nil)))


(provide 'kzk-org-babel)
