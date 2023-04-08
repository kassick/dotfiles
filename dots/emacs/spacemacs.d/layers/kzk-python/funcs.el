(defun kzk/py-refactor-extract-arg (new-name)
  "Extracts an argument to a variable immediately above the current statement"
  (interactive "sVariable Name: ")

  (let* ((bounds (if (region-active-p)
                     (car (region-bounds)) ;; no support for non contiguous regions ...
                   (let* ((inner-arg-bounds (evil-inner-arg))
                          (start (evil-range-beginning inner-arg-bounds))
                          (end (evil-range-end inner-arg-bounds)))
                     (cons start end))))
         (start (car bounds))
         (end (cdr bounds))
         (text (buffer-substring-no-properties start end))
         (start-of-arg-marker (make-marker))
         (cur-statement-end-marker (make-marker))
         )
    ;; Save start of current argument position, we'll move the cursor to this
    ;; position later
    (set-marker start-of-arg-marker start)

    ;; Substitute the current argument text with the provided variable name
    (replace-region-contents start end (lambda () new-name))

    ;; Figure current statement's boundaries
    (let ((statement-start (save-excursion
                             (python-nav-beginning-of-statement)
                             (point)))
          (statement-end (save-excursion
                           (python-nav-end-of-statement)
                           (point))))

      ;; Leave a mark at the end of the current function call
      (set-marker cur-statement-end-marker statement-end)

      ;; Insert new variable atribution before
      (goto-char statement-start)
      (insert (concat new-name " = " text))
      (newline-and-indent)

      ;; Indent from the point there whe new statement was inserted up to the
      ;; end of the function call where the refactor was called
      (indent-region statement-start (marker-position cur-statement-end-marker)))

    ;; Place the cursor at the start of the argument that was extracted
    (goto-char (marker-position start-of-arg-marker))

    ;; clear markers
    (set-marker start-of-arg-marker nil)
    (set-marker cur-statement-end-marker nil)))

(defun kzk/pyenv-mode-set-local-virtualenv ()
  "If a pyenv .python-version file is available in the directory tree, use it as pyvenv-activate"
  (interactive)
  (message "kzk find pyenv python version")
  (when-let* ((root-path (locate-dominating-file default-directory ".python-version"))
              (file-path (expand-file-name ".python-version" root-path)))
    (message "found stuff %S" file-path)
    (when (file-regular-p file-path)
      (message "regular file")
      (let* ((venv-name (with-temp-buffer
                         (insert-file-contents file-path)
                         (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
             (venv-full-path (concat (expand-file-name "~/.pyenv/versions") "/" venv-name)))
        (message "found venv name %S at %S" venv-name venv-full-path)
        (make-variable-buffer-local 'pyvenv-activate)
        (setq pyvenv-activate  venv-full-path)))))

(defun kzk/spacemacs-pyenv-mode-set-local-virtualenv (orig-fun &rest args)
  (message "in kzk set local virtualenv")
  (when (not (kzk/pyenv-mode-set-local-virtualenv))
    (message "original func")
    (apply orig-fun args)))
