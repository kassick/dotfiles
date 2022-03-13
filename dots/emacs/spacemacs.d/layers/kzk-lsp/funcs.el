(defun kzk/lsp-help-at-point (&optional arg)
  "Displays lsp-help for thing at point in a help window. When
called with a prefix, kills the window"
  (interactive "P")

  (if arg
      ;;; Calling with prefix, kill help window
      (let* ((help-buffer (get-buffer "*lsp-help*"))
             (help-window (when help-buffer (get-buffer-window help-buffer))))
        (when help-window
          (delete-window help-window)))
    ;;; called without prefix, describe at point
    (let ((help-window-select nil))
      (lsp-describe-thing-at-point))))
