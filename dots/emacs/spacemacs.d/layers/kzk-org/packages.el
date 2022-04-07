(defconst kzk-org-packages
  '((org :location elpa :min-version "9.5")
    (evil-org :location elpa)
    helm-org

    ))

(defun kzk-org/post-init-org ()
  ;; lazy load to avoid starting org on boot
  (with-eval-after-load 'org
    ;; babel
    (kzk/setup-org-babel)

    ;; fix fonts insisting to be height 1.2
    (kzk/org-fix-org-header-fonts)

    ;; Extra packages for org-mode
    (require 'org-tempo)

    ;; org export
    (require 'ox-extra)
    (ox-extras-activate '(ignore-headlines))

    (with-eval-after-load 'ox-latex
      ;; (message "Loading custom settings for tex")
      (unless (boundp 'org-latex-default-packages-alist)
        (setq org-latex-default-packages-alist nil))
      (add-to-list 'org-latex-default-packages-alist '("" "ifxetex" t))
      (add-to-list 'org-latex-default-packages-alist '("" "minted" t))
      )


    (add-hook 'org-mode-hook 'org-display-inline-images)
    (add-hook 'org-mode-hook 'embrace-org-mode-hook)

    (general-define-key :keymaps 'org-mode-map
                        "C-c d" 'kzk/insert-date
                        "C-c t" 'kzk/insert-time-date
                        "C-c v" 'org-show-todo-tree
                        "C-c A c" '(kzk/add-current-file-to-org-capture :which-key: "adds current file to org-capture-templates")
                        "C-c A a" '(kzk/add-current-file-to-org-agenda :which-key: "adds current file to org-agenda-files")))
  (kzk/after-init
   (general-define-key :keymaps 'global
                       "C-c C" 'kzk/org-capture)))


;; Spacemacs layer hooks run too late -- add this hook right after init
(kzk/after-init
 (with-eval-after-load 'helm-org
   ;; redefine helm-org-indent-headings to sort candidates by position
   ;; helm-org tries to use helm-generic-sort-fn as metadata sort-fn -- but sort-fn is only used
   ;; in emacs style completion -- and spacemacs uses helm style completion
   (defun helm-org-indent-headings (candidates _source)
     "Indent headings and hide leading stars displayed in the helm buffer.
If `org-startup-indented' and `org-hide-leading-stars' are nil, do
nothing to CANDIDATES."

     (let  ((indented-candidates
             (cl-loop for disp in candidates collect
                      (helm-org-indent-headings-1 disp)) ) )
       (message "sorting candidates by my function")
       (sort indented-candidates (lambda (c1 c2)
                                   (let ((pos-c1 (get-text-property 0 'helm-realvalue c1))
                                         (pos-c2 (get-text-property 0 'helm-realvalue c2)))
                                     (< pos-c1 pos-c2))))))))

(defun kzk-org/post-init-evil-org ())
