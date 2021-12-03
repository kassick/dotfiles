(require 'kzk-utils)
(require 'kzk-org-common)

(require 'kzk-org-capture)
(require 'kzk-org-agenda)

;;; {{{ Set behaviour for links
;;;     Use kzk/open-fun by default for system.
;;;     May be changed by customized
(defvar org-file-apps (quote (
                              ;; (directory . system)   ; uncomment to open directories with xdg-open always
                              (t . emacs)              ; set to system to have xdg-open by default
                              (auto-mode . emacs)
                              ;;; (system . "/usr/bin/xdg-open \"%s\" </dev/null")
                              (system . (lambda (file link) (kzk/open-fun file)))
                              ("\\.mm\\'" . default)
                              ("\\.x?html?\\'" . default)
                              ("\\.pdf\\'" . default))))
;; }}}


(with-eval-after-load 'org
  (with-eval-after-load 'general
   (general-define-key :keymaps 'org-mode-map
                       "C-c d" 'kzk/insert-date
                       "C-c t" 'kzk/insert-time-date
                       "C-c v" 'org-show-todo-tree))

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
        org-default-notes-file (kzk/org-directory-file "notes.org")
        org-agenda-include-all-todo t
        org-agenda-include-diary t
        ;; org-export-babel-evaluate nil
        org-confirm-babel-evaluate nil
        org-capture-bookmark nil
        org-startup-indented t)

  ;;; allow ordered lists to use a. a) etc. as items.
  ;;; this value must be set before org is loaded, otherwise we
  ;;; must call org-element-update-syntax
  (setq org-list-allow-alphabetical t)
  (org-element-update-syntax)

  (add-hook 'org-mode-hook 'org-display-inline-images)
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)

  (require 'kzk-org-babel)

  ;; Extra packages for org-mode
  (require 'org-tempo)
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  ;;(require 'org-protocol)
  )

;; LaTeX settings
(with-eval-after-load 'ox-latex
  ;; (message "Loading custom settings for tex")
  (unless (boundp 'org-latex-default-packages-alist)
    (setq org-latex-default-packages-alist nil))
  (add-to-list 'org-latex-default-packages-alist '("" "ifxetex" t))
  (add-to-list 'org-latex-default-packages-alist '("" "minted" t))
  )

(with-eval-after-load 'helm-org
  ;;; redefine helm-org-indent-headings to sort candidates by position
  ;;; helm-org tries to use helm-generic-sort-fn as metadata sort-fn -- but sort-fn is only used
  ;;; in emacs style completion -- and spacemacs uses helm style completion
  (defun helm-org-indent-headings (candidates _source)
    "Indent headings and hide leading stars displayed in the helm buffer.
If `org-startup-indented' and `org-hide-leading-stars' are nil, do
nothing to CANDIDATES."
    (let  ((indented-candidates
            (cl-loop for disp in candidates collect
                     (helm-org-indent-headings-1 disp)) ) )
      (sort indented-candidates (lambda (c1 c2)
                                  (let ((pos-c1 (get-text-property 0 'helm-realvalue c1))
                                        (pos-c2 (get-text-property 0 'helm-realvalue c2)))
                                    (< pos-c1 pos-c2))))))
  )

(provide 'kzk-org)
