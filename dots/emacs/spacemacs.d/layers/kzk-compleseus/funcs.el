(defun kzk/consult-descbinds--all-sections (buffer &optional prefix menus)
  "Collect data from `describe-buffer-bindings' output.

Return a list of sections, each section beeing an alist composed of
\(KEY . COMMAND)."

  ;; Fetched and adapted from helm-descbinds

  (with-temp-buffer
    (let ((indent-tabs-mode t))
      (describe-buffer-bindings buffer prefix menus))
    (goto-char (point-min))
    (let ((header-p (not (= (char-after) ?\f))) ;; ?\f == ^L
          sections
          header
          section)
      (while (not (eobp))
        (cond
         (header-p
          (setq header (buffer-substring-no-properties
                        (point) (line-end-position)))
          (setq header-p nil)
          (forward-line 3))
         ((= (char-after) ?\f) ;; ?\f == ^L
          (push (cons header (nreverse section)) sections)
          (setq section nil)
          (setq header-p t))
         ((looking-at "^[ \t]*$")) ;; ignore
         (t
          (let ((binding-start (save-excursion
                                 (and (re-search-forward "\t+" nil t)
                                      (match-end 0))))
                key binding)
            (when binding-start
              ;; For some reasons on Emacs-29 key description is
              ;; sometimes 2 lines long, it seems it happens with menus
              ;; but `describe-buffer-bindings' is always called with
              ;; MENUS == nil...?
              (setq key (car (split-string
                              (buffer-substring-no-properties
                               (point) binding-start)
                              "\n" t))
                    key (replace-regexp-in-string "^[ \t\n]+" "" key)
                    key (replace-regexp-in-string "[ \t\n]+$" "" key))
              (goto-char binding-start)
              (setq binding (buffer-substring-no-properties
                             binding-start
                             (line-end-position)))
              (unless (member binding '("self-insert-command"))
                (push (cons key binding) section))))))
        (forward-line))
      (push (cons header (nreverse section)) sections)
      (nreverse sections))))


(defcustom kzk/consult-descbinds-section-order
  '("Major Mode Bindings" "Minor Mode Bindings" "Global Bindings")
  "A list of section order by name regexp."
  :type '(repeat (regexp :tag "Regexp")))

(defun kzk/consult-descbinds--order-section (section)
  "Return the number in which SECTION should appear.

This is used to reorder all sections as sources."
  (cl-loop for n = 0 then (1+ n)
           for regexp in kzk/consult-descbinds-section-order
           if (and (car section) (string-match regexp (car section)))
           return n
           finally
           return n))

(defun kzk/consult-descbinds--format-candidates (candidates)
  (let* ((key-width (cl-loop for (key . _def) in candidates
                         maximize (length key))))
    (cl-loop for (key . def) in candidates
             for key-rep = (concat (propertize key 'face 'embark-keybinding))
             for formatted = (propertize (concat key-rep
                                                 (make-string (- key-width (length key-rep) -1) ?\s)
                                                 def)
                                         'embark-command (intern def)
                                         )
             collect (cons formatted def)
             )
    )
  )

(defun kzk/consult-descbinds--sources (buffer &optional prefix menus)

  (-let* ((all-sections-candidates (kzk/consult-descbinds--all-sections buffer prefix menus))
          (filtered-sections (-filter (lambda (e)
                                        (and e
                                             (car e)
                                             (not (string-match "translations:$" (car e)))))
                                      all-sections-candidates))
          (sorted (sort filtered-sections
                        (lambda (a b)
                          (< (kzk/consult-descbinds--order-section a)
                             (kzk/consult-descbinds--order-section b))))))
    (mapcar (lambda (section)
              (-let* ( ((name . candidates) section)
                       (formatted-candidates (kzk/consult-descbinds--format-candidates candidates)))
                `(:name ,name
                        :category embark-keybinding
                        :items ,(kzk/consult-descbinds--format-candidates candidates))))
            sorted)))



(defun kzk/consult-descbinds (&optional prefix buffer)
  (interactive)

  (require 'consult)
  (require 'embark)
  (require 'marginalia)
  (let* ((selected (consult--multi (kzk/consult-descbinds--sources (or buffer (current-buffer)) prefix)
                                  :require-match t
                                  :category 'embark-keybinding
                                  :prompt "Bindings: "
                                  :sort nil)  )
          )
    (command-execute (intern (car selected)))
    )  )
