;;;  -*- lexical-binding: t -*-

(defun kzk/--is-window-mode-a (wnd mode)
  (with-current-buffer (window-buffer wnd)
    (derived-mode-p mode)))


(defun kzk/--delete-mode-windows (mode &optional all)
  "Deletes the windows of mode.

  When all it t, delete
"
  (let* ((predicate (lambda (wnd) (kzk/--is-window-mode-a wnd mode)))
         (windows-to-kill
          (if all
              (-filter predicate (window-list))
            (let ((wnd (-first predicate (window-list))))
              (when wnd
                (list wnd))))))
    (-map (lambda (wnd) (when wnd (delete-window wnd)))
          windows-to-kill)
    (message "%s windows killed" (length windows-to-kill))))


(defun kzk/delete-help-window (&optional prefix)
  "Deletes the all windows displaying a help buffer

  With a prefix, deletes only one"

  (interactive "P")
  (kzk/--delete-mode-windows 'help-mode (not prefix)))

(defun kzk/delete-compile-window (&optional prefix)
  "Deletes all compilation windows.

  With a prefix, deletes only one"

  (interactive "P")
  (kzk/--delete-mode-windows 'compilation-mode (not prefix)))

(defun helm-esw/show-buffer (candidate)
  (set-window-buffer (esw/select-window nil t t) (get-buffer candidate)))


(defun helm-esw/find-file-splitted-window (filename)
  "Use esw to select the target window for filename"
  (let* ((buffer (find-file-noselect filename))
         (new-window (esw/select-window nil t t)))
    (set-window-buffer new-window buffer)
    (select-window new-window)

    buffer))

(defun helm-esw/ag-find-file (candidate)
  "Selects a target window with esw before finding the file"
  (helm-ag--find-file-action candidate 'helm-esw/find-file-splitted-window (helm-ag--search-this-file-p)))

(defun helm-esw/run-ag-find-file ()
  "Show the selected match in the selected window"
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-esw/ag-find-file)))

(defun helm-esw/find-file (candidate)
  (set-window-buffer (esw/select-window nil t t) (find-file-noselect candidate)))

(defun helm-esw/run-show-buffer ()
  "Show the selected buffer in the selected window."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-esw/show-buffer)))

(defun helm-esw/run-find-file ()
  "Show the selected buffer in the selected window."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-esw/find-file)))

(defun kzk/setup-helm-esw ()
  ;; find file
  (add-to-list 'helm-find-files-actions '("Find file in in new splited window `C-c w'" . helm-esw/find-file ) t)
  (define-key helm-find-files-map (kbd "C-c w") 'helm-esw/run-find-file)

  (with-eval-after-load 'projectile
    (helm-projectile-on)
    ;; projectille ff is in a different map
    (define-key helm-projectile-find-file-map (kbd "C-c w") 'helm-esw/run-find-file))

  ;; file type actions
  (add-to-list 'helm-type-file-actions '("Find file in in new splited window `C-c w'" . helm-esw/find-file ) t)
  (define-key helm-generic-files-map (kbd "C-c w") 'helm-esw/run-find-file)

  ;; buffers
  (add-to-list 'helm-type-buffer-actions
               '("Display buffer(s) in new splited window `C-c w'" . helm-esw/show-buffer) t)
  (define-key helm-buffer-map (kbd "C-c w") 'helm-esw/run-show-buffer)

  ;; helm ag
  (with-eval-after-load 'helm-ag
    (add-to-list 'helm-ag--actions
                 '("Find match in in new splited window `C-c w'" . helm-esw/ag-find-file ) t)
    (define-key helm-ag-map (kbd "C-c w") 'helm-esw/run-ag-find-file)))

;;; Taken from https://karthinks.com/software/fifteen-ways-to-use-embark/
;;; (and adapted for names)
;;; {{{
(eval-when-compile
  (defmacro kzk/embark-ace-action (fn)
    `(defun ,(intern (concat "kzk/embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "Ace Embark: %s"
         (require 'ace-window)
         (let ((aw-dispatch-always t))
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn)))))))
;;; }}}

(defun kzk/esw-ff (filename &rest args)
  "Finds a file and open it in a ESW selected window"
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))

  ;; use ace-window, as it ends up forcing redisplays -- this works better
  ;; when lsp or some other mode init function requires user input
  (require 'ace-window)
  (let* ((wnd (aw-switch-to-window (esw/select-window nil t t) ))
         (value (find-file-noselect filename nil nil nil)))
    (set-window-buffer wnd value)))

(defun kzk/esw-buffer (buffer-or-name &rest args)
  "Opens a buffer in a ESW selected window"
  (interactive
   (list (read-buffer-to-switch "Switch to buffer in other window: ")))

  (require 'ace-window)
  (let* ((wnd (aw-switch-to-window (esw/select-window nil t t) )))
    (set-window-buffer wnd buffer-or-name)))

(defun kzk/embark-grep-action-other-window (location)
  "Finds the entry at other window.

   This function is not interactive because location is a prop
   text -- could not find an interactive parameter that could
   keep the parameter and work correctly with embark.
   "

  (require 'ace-window)

  (pop-to-buffer (current-buffer) t nil)
  (embark-consult-goto-grep location)
  (let ((window (selected-window)))
    ;; Something is restoring window focus after the command finishes
    ;; Maybe because this function is not interactive...
    (run-at-time 0 nil (lambda () (aw-switch-to-window window)))))

(defun kzk/embark-grep-action-other-frame (location)
  "Finds the entry at other frame."
  ;; simple, apparently works
  (switch-to-buffer-other-frame (current-buffer))
  (embark-consult-goto-grep location)
  ;; do we need to enfore selected window?

  ;;; more explicit ...
  ;; (let* ((cur-buf (current-buffer))
  ;;        (target-window (with-selected-frame (make-frame)
  ;;                         (switch-to-buffer cur-buf)
  ;;                         (embark-consult-goto-grep location)
  ;;                         (selected-window))))
  ;;   (require 'ace-window)
  ;;   (aw-switch-to-window target-window)
  ;;   )
  ;;
  ;; weird but also works
  ;; (save-window-excursion
  ;;   (save-excursion
  ;;     (embark-consult-goto-grep location)
  ;;     (switch-to-buffer-other-frame (current-buffer))
  ;;   ))
  )


(defun kzk/embark-grep-action-esw (location)
  "Opens a candidate in a ESW selected window"
  ;; This is convoluted, but that's how I got it to work ...

  ;; Save the current path (if we ESW-select a window visiting a file in
  ;; other path, we may end up opening the file in the invalid path, as
  ;; location is relative).
  ;;
  ;; The path will be updated by consult-grep to be wither the project root or
  ;; whatever target directory is used
  (let ((path default-directory))

    ;; Then run the whole setup on a timer, otherwise focus does not work ...
    (run-at-time 0 nil (lambda ()
                         ;; Save the target window
                         (let ((wnd (esw/select-window nil t t)))
                           ;; ensure wnd is selected
                           (require 'ace-window)
                           (aw-switch-to-window wnd)

                           ;; and then call embark-consult-goto-grep with the
                           ;; window selected
                           (with-selected-window wnd
                             ;; go to the base path
                             (cd path)
                             (embark-consult-goto-grep location))))))
  ;; does not focus ...
  ;; (let* ((buffer-at-point (save-excursion
  ;;                           (save-window-excursion
  ;;                             (embark-consult-goto-grep location)
  ;;                             `(,(current-buffer) . ,(point)))))
  ;;        (buf (car buffer-at-point))
  ;;        (p (cdr buffer-at-point))
  ;;        (wnd (esw/select-window nil t t)))
  ;;   (require 'ace-window)
  ;;   (aw-select wnd)
  ;;   (set-window-buffer wnd buf)
  ;;   (goto-char p)
  ;;   (run-at-time 0 nil 'aw-select wnd)
  ;;   )
  )

(defun kzk/handle-delete-frame-error (orig-fun &rest args)
  (message "Handling possible posframe issue on delete frame")
  (condition-case err
      (apply orig-fun args)
    ((debug error)
     (progn
       (posframe-delete-all)
       (apply orig-fun args)))))
