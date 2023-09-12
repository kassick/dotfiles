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

;; Taken from https://github.com/seagle0128/.emacs.d/commit/a73a46ef3630e2492f5a0d70c32a59fead6c2ed6
;; Show peek on a posframe and avoid narrow peek windows.
;; Upstream issue: https://github.com/emacs-lsp/lsp-ui/issues/441
(defun kzk/childframe-workable-p ()
  "Whether childframe is workable."
  (not (or noninteractive
           emacs-basic-display
           (not (display-graphic-p)))))

(defun kzk/lsp-ui-peek--peek-display (fn src1 src2)
  (if (and kzk/peek-uses-posframe (kzk/childframe-workable-p))
      (-let* (
              (f-width (frame-width))
              (win-width (truncate (min
                                    ;; never wider than the frame
                                    f-width
                                    ;; If an absolute max width set
                                    (or kzk/max-peek-width f-width)
                                    ;; If set, a proportional max width
                                    (* (or kzk/max-peek-width-proportional 1) f-width)
                                    )))
                       ;; (lsp-ui-peek-list-width (/ win-width 2))
                       (string (-some--> (-zip-fill "" src1 src2)
                                 (--map (lsp-ui-peek--adjust win-width it) it)
                                 (-map-indexed 'lsp-ui-peek--make-line it)
                                 (-concat it (lsp-ui-peek--make-footer)))))
        (setq lsp-ui-peek--buffer (get-buffer-create " *lsp-peek--buffer*"))
        (posframe-show lsp-ui-peek--buffer
                       :string (mapconcat 'identity string "")
                       :min-width win-width
                       :max-width win-width
                       :min-height lsp-ui-peek-peek-height
                       :max-height lsp-ui-peek-peek-height
                       :internal-border-color (face-foreground 'font-lock-comment-face nil t)
                       :internal-border-width 3
                       :poshandler #'posframe-poshandler-frame-center))
    (funcall fn src1 src2)))

(defun kzk/lsp-ui-peek--peek-destroy (fn)
  (if (and kzk/peek-uses-posframe (kzk/childframe-workable-p))
      (progn
        (when (bufferp lsp-ui-peek--buffer)
          (posframe-hide lsp-ui-peek--buffer))
        (setq lsp-ui-peek--last-xref nil))
    (funcall fn)))
