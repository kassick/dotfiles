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
                                    (* (or (and (> f-width kzk/max-peek-width-proportional-min-width) kzk/max-peek-width-proportional) 1) f-width)
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

(defun kzk/lsp-workspaces-gc()
  "Delete all lsp tracked folders that no longer exists."
  (interactive)
  (let ((removed nil))
    (--each (lsp-session-folders (lsp-session))
      (unless (file-directory-p it)
        (lsp-workspace-folders-remove it)
        (setq removed t)
        ))
    (unless removed (message "No folder removed"))))


;; (defun kzk/deep-merge-alists (alist1 alist2)
;;   "Deep merge two association lists ALIST1 and ALIST2."
;;   (let ((result (copy-alist alist1)))
;;     (dolist (pair alist2 result)
;;       (let ((key (car pair))
;;             (value (cdr pair)))
;;         (if (assoc key result)
;;             (let ((existing-value (cdr (assoc key result))))
;;               (setcdr (assoc key result)
;;                       (if (and (listp existing-value) (listp value))
;;                           (kzk/deep-merge-alists existing-value value)
;;                         value)))
;;           (push pair result))))))

;; (defun kzk/lsp--client-capabilities-advice (fn &optional custom-capabilities)
;;   (kzk/deep-merge-alists (apply fn '()) custom-capabilities))

(defun kzk/lsp-warn-advice (fn message &rest args)
  "Stops warning about unknown request methods -- this ends up spamming the user"
  (let* ((call-args (append (list message) args))
         (min-level (if (string-match-p "unknown request method:" (downcase message))
                        :emergency
                      warning-minimum-level))
         (warning-minimum-level min-level))
    (apply fn call-args)))

(defun kzk/lsp-toggle-inlay-hints ()
  "Toggle LSP Inlay Hints"
  (interactive)
  (if lsp-inlay-hint-enable
      (progn
        (lsp-inlay-hints-mode -1)
        (setq lsp-inlay-hint-enable nil))
    (lsp-inlay-hints-mode t)
    (setq lsp-inlay-hint-enable t))

  (lsp))
(defun kzk/lsp-workspace-command-execute (command &optional args)
  "Execute workspace COMMAND with ARGS."
  (condition-case-unless-debug err
      (let ((params (if args
                        (list :command command :arguments args)
                      (list :command command)))
            (ws (lsp--find-workspaces-for "workspace/execute-command")))
        (cl-loop for w in ws do
                 (with-lsp-workspace w
                   (when (lsp-can-execute-command? command)
                     (lsp-request "workspace/executeCommand" params)))))
    (error
     (error "`workspace/executeCommand' with `%s' failed.\n\n%S"
            command err))))

