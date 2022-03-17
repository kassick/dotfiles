(defmacro kzk/execute-and-get-point-or-bail (&rest FORMS)
  `(let ((opoint (point)))
     (or (boundp 'bail) (setq bail -1))
     ;; (message "bailing to %S" bail)
     (save-excursion
       (eval ,@FORMS)
       ;;(message "moved to %S from %S" (point) opoint)
       (if (= (point) opoint)
           bail
         (point)))))

(defun kzk/beginning-of-visual-line-or-indent (&optional n)
  "Move point to closest point among:
  - beginning of current visual line
  - indent
  - first character
  "
  (interactive "^p")

  (or n (setq n 1))
  (when (/= n 1)
    (let ((line-move-visual t))
      (line-move (1- n) t)))

  (let* (
         (point-field
          (kzk/execute-and-get-point-or-bail (field-beginning)))

         (point-starting-of-visual-line
          (kzk/execute-and-get-point-or-bail (when (bound-and-true-p visual-line-mode)
                                               (vertical-motion 0))))

         (point-indent
          ;; back-to-indentation moves to the next char after indentation. In a
          ;; blank line, this will always move to the next char, resultin in
          ;; this movement never bailing
          (kzk/execute-and-get-point-or-bail (when (not (looking-back "^[[:space:]]\+"))
                                               (back-to-indentation))))

         (point-starting-of-line
          (kzk/execute-and-get-point-or-bail (move-beginning-of-line 1)))

         (target (max point-field point-starting-of-visual-line point-indent point-starting-of-line)))

    ;; (message "cur: %S field: %S visual: %S indent: %S line: %S" (point) point-field point-starting-of-visual-line point-indent point-starting-of-line)

    (when (/= target -1)
      (goto-char target))) )

(defun kzk/end-of-visual-line-or-eol (&optional n)
  "Move point to the closest point among:
  - end of visual line
  - end of line
  "
  (interactive "^p")
  (or n (setq n 1))

  (when (/= n 1)
    (let ((line-move-visual t))
      (line-move (1- n) t)))

  (let* ((bail (1+ (point-max)))

         (point-end-of-field
          (kzk/execute-and-get-point-or-bail (field-end)))

         (point-end-of-visual-line
          (kzk/execute-and-get-point-or-bail (when (bound-and-true-p visual-line-mode)
                                                    (vertical-motion (cons (window-width) 0)))))

         (point-end-of-line-non-blank
          (kzk/execute-and-get-point-or-bail (evil-last-non-blank)
                                             (right-char 1)
                                             ))

         (point-end-of-line
          (kzk/execute-and-get-point-or-bail (end-of-line 1)))

         (target (min point-end-of-field point-end-of-visual-line
                      point-end-of-line-non-blank
                      point-end-of-line)))

    ;; (message "target: %S field: %S visual: %S target line-non-blank: %S eol: %S" target point-end-of-field point-end-of-visual-line point-end-of-line-non-blank point-end-of-line)
    (when (/= target bail)
      (goto-char target))
    ))



(global-set-key (kbd "<home>") 'kzk/beginning-of-visual-line-or-indent)
(global-set-key (kbd "<end>") 'kzk/end-of-visual-line-or-eol)

(with-eval-after-load 'general
  (general-define-key :states '(normal visual motion insert)
                      "<home>" 'kzk/beginning-of-visual-line-or-indent
                      "<end>" 'kzk/end-of-visual-line-or-eol))

(provide 'kzk-smart-home-end)
