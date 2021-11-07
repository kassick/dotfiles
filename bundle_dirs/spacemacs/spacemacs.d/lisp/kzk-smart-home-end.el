(defun kzk/beginning-of-visual-line-or-indent (&optional n)
  "Move point to closest point among:
  - beginning of current visual line
  - indent
  - first character
  "
  (interactive "^p")
  (or n (setq n 1))
  (let ((opoint (point)))
    (when (/= n 1)
      (let ((line-move-visual t))
        (line-move (1- n) t)))

    (defun point-or-bail ()
      (if (= (point) opoint)
          -1
        (point))
      )

    (let ((target
           (save-excursion
             (max (progn
                    (vertical-motion 0)
                    (goto-char (constrain-to-field (point) opoint (/= n 1)))

                    (point-or-bail))
                  (progn
                    (back-to-indentation)
                    (point-or-bail))

                  (progn
                    (move-beginning-of-line 1)
                    (point-or-bail))
           ))))

      (if (/= target -1)
          (goto-char target)))))

(defun kzk/end-of-visual-line-or-eol (&optional n)
  "Move point to the closest point among:
  - end of visual line
  - end of line
  "
  (interactive "^p")
  (or n (setq n 1))
  (let ((opoint (point)))
    (when (/= n 1)
      (let ((line-move-visual t))
        (line-move (1- n) t)))
    ;; Unlike `move-beginning-of-line', `move-end-of-line' doesn't
    ;; constrain to field boundaries, so we don't either.
    (vertical-motion (cons (window-width) 0))

    (when (= opoint (point))
      (end-of-line 1))))


(global-set-key (kbd "<home>") 'kzk/beginning-of-visual-line-or-indent)
(global-set-key (kbd "<end>") 'kzk/end-of-visual-line-or-eol)

(with-eval-after-load 'general
  (general-define-key :states '(normal visual motion insert)
                      "<home>" 'kzk/beginning-of-visual-line-or-indent
                      "<end>" 'kzk/end-of-visual-line-or-eol))

(provide 'kzk-smart-home-end)
