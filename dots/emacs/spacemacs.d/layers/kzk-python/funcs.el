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
