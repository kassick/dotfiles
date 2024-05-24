(defun kzk/markdown-in-footnote-marker-p ()
  (and (markdown-footnote-marker-positions) t))

(defun kzk/markdown-in-footnote-text-p ()
  (and (markdown-footnote-text-positions) t))

(defun kzk/markdown-follow-at-point (fn &rest args)
  (cond ((kzk/markdown-in-footnote-marker-p) (markdown-footnote-goto-text))
        ((kzk/markdown-in-footnote-text-p) (markdown-footnote-return))
        (t (apply fn args)))
  )
