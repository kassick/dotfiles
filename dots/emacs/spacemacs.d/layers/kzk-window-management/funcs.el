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

;;; ----------------------------------------------------------------------
;;; Helm actions
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

;;; ----------------------------------------------------------------------
;;; Vertico / Consult / Embark

(defmacro kzk/defun-embark-action-on-other-window (fn-name arg-list help &rest FORMS)
  (declare (indent defun))
  `(defun ,fn-name ,arg-list
       ,help
     (switch-to-buffer-other-window (current-buffer))
     ,@FORMS
     (require 'ace-window)
     (run-at-time 0 nil #'aw-switch-to-window (selected-window))))


(defmacro kzk/defun-embark-action-on-other-frame (fn-name arg-list help &rest FORMS)
  (declare (indent defun))
  `(defun ,fn-name ,arg-list
       ,help
     (switch-to-buffer-other-frame (current-buffer))
     ,@FORMS))

(defmacro kzk/defun-embark-action-esw (fn-name arg-list help &rest FORMS)
  (declare (indent defun))
  `(defun ,fn-name ,arg-list
     ,help
     (let ((orig-buffer (current-buffer)))
       (esw/select-window nil t t)
       (switch-to-buffer orig-buffer)
       ,@FORMS
       (require 'ace-window)
       (run-at-time 0 nil #'aw-switch-to-window (selected-window)))))

(defmacro kzk/defun-embark-action-ace (fn-name arg-list help &rest FORMS)
  (declare (indent defun))
  `(defun ,fn-name ,arg-list
     ,help

     (let ((orig-buffer (current-buffer)) )
       (ace-select-window)
       (switch-to-buffer orig-buffer nil 'force-same-window)
       (message " a selected window: %S" (selected-window))
       ,@FORMS
       (require 'ace-window)
       (run-at-time 0 nil #'aw-switch-to-window (selected-window)))))

;;; Taken from https://karthinks.com/software/fifteen-ways-to-use-embark/
;;; (and adapted for names)
;;; {{{
(eval-when-compile
  (defmacro kzk/embark-ace-action (fn)
    `(defun ,(intern (concat "kzk/embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "Ace Embark: %s"
         (require 'ace-window)
         (aw-switch-to-window (ace-select-window))
         (call-interactively (symbol-function ',fn))))))
;;; }}}

;;; Embark emacs buffer and file
;;;
;;; These functions are handy as embark actions on files/buffers to call
;;; either ace or esw

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
  "Select window (es-windows) and switch to buffer"
  (interactive
   (list (read-buffer-to-switch "Switch to buffer in other window: ")))

  (require 'ace-window)
  (let* ((wnd (aw-switch-to-window (esw/select-window nil t t) )))
    (set-window-buffer wnd buffer-or-name)))

;;; Embark Grep results
;;;
;;; These add {other-window, other-frame, esw, ace} actions for grep results

(defun kzk/embark-grep-action-other-window (location)
  "Select window (es-windows) where to find match"

  ;; This function is not interactive because location is a prop, and embark
  ;; loses the properties when calling interactive functions (it explicitly
  ;; inserts the text in the minibuffer, as if the function had been called by
  ;; the user).

  (pop-to-buffer (current-buffer) t nil)
  (embark-consult-goto-grep location)

  ;; Something is restoring window focus after the command finishes
  ;; Maybe because this function is not interactive...
  (require 'ace-window)
  (run-at-time 0 nil #'aw-switch-to-window (selected-window)))

(defun kzk/embark-grep-action-other-frame (location)
  "Finds match at other frame"

  (switch-to-buffer-other-frame (current-buffer))
  (embark-consult-goto-grep location))

(defun kzk/embark-grep-action-esw (location)
  "Select window (es-windows) where to find match"
  ;; This is convoluted, but that's how I got it to work ...

  ;; Save the current path (if we ESW-select a window visiting a file in
  ;; other path, we may end up opening the file in the invalid path, as
  ;; location is relative).
  ;;
  ;; The path will be updated by consult-grep to be within the project root or
  ;; whatever target directory is used

  (let ((path default-directory))
    (esw/select-window nil t t)
    (cd path)
    (embark-consult-goto-grep location)

    ;;; Ensure the window is selected after the interactive command is completed
    (require 'ace-window)
    (run-at-time 0 nil #'aw-switch-to-window (selected-window))))


(defun kzk/embark-grep-action-ace (location)
  "Select window (ace) where to find match"
  ;; This is convoluted, but that's how I got it to work ...

  ;; Save the current path (if we ESW-select a window visiting a file in
  ;; other path, we may end up opening the file in the invalid path, as
  ;; location is relative).
  ;;
  ;; The path will be updated by consult-grep to be within the project root or
  ;; whatever target directory is used

  (require 'ace-window)
  (let ((path default-directory)
        ;; (aw-dispatch-always t)
        )

    (ace-select-window)
    (cd path)
    (embark-consult-goto-grep location)

    ;;; Ensure the window is selected after the interactive command is completed
    (run-at-time 0 nil #'aw-switch-to-window (selected-window))))

;;; Embark Location results
;;;
;;; Location is not grep!!! Location includes stuff suth as the results of
;;; consult-line & friends
;;;
;;; These functions add {other-frame, other-window, esw, ace} actions to
;;; location results

(defun kzk/embark-consult-location-esw (location)
  "Select window (es-windows) where to find location"
  (let ((location-mark (car (get-text-property 0 'consult-location location) )))
    (esw/select-window nil t t)
    (switch-to-buffer (marker-buffer location-mark))
    (goto-char location-mark)

    (require 'ace-window)
    (run-at-time 0 nil #'aw-switch-to-window (selected-window))))

(defun kzk/embark-consult-location-ace (location)
  "Select window (ace) where to find location"

  (require 'ace-window)
  (let ((location-mark (car (get-text-property 0 'consult-location location) )))
    (ace-select-window)
    (switch-to-buffer (marker-buffer location-mark))
    (goto-char location-mark)

    (run-at-time 0 nil #'aw-switch-to-window (selected-window))))

(defun kzk/embark-consult-location-other-window (location)
  "Finds location on other window"
  (let ((location-mark (car (get-text-property 0 'consult-location location) )))
    (pop-to-buffer (marker-buffer location-mark) t nil)
    (goto-char location-mark)

    (require 'ace-window)
    (run-at-time 0 nil #'aw-switch-to-window (selected-window))))

(defun kzk/embark-consult-location-other-frame (location)
  "Finds location on other frame"
  (let ((location-mark (car (get-text-property 0 'consult-location location) )))
    (switch-to-buffer-other-frame (marker-buffer location-mark))
    (goto-char location-mark)))

;;; Embark xref
;;;
;;; These functions add support for {other-window, other-frame, esw, ace}
;;; actions for xref results

(kzk/defun-embark-action-esw kzk/embark-consult-xref-esw (reference)
  "Select window (es-windows) where to find xref"
  (let ((xref-reference (get-text-property 0 'consult-xref reference)))
    (xref-pop-to-location xref-reference)))

(kzk/defun-embark-action-ace kzk/embark-consult-xref-ace (reference)
  "Select window (ace) where to find xref"
  (let ((xref-reference (get-text-property 0 'consult-xref reference)))
    (xref-pop-to-location xref-reference)))


(kzk/defun-embark-action-on-other-window kzk/embark-consult-xref-other-window (reference)
  "Finds xref on other window"
  (let ((xref-reference (get-text-property 0 'consult-xref reference)))
    (xref-pop-to-location xref-reference)))

(kzk/defun-embark-action-on-other-frame kzk/embark-consult-xref-other-frame (reference)
  "Finds xref on other frame !"
  (let ((xref-reference (get-text-property 0 'consult-xref reference)))
    ;; Do not use the 'frame action, as that tries to reuse frames
    (xref-pop-to-location xref-reference)))

;;; ----------------------------------------------------------------------

(defun kzk/handle-delete-frame-error (orig-fun &rest args)
  (condition-case err
      (apply orig-fun args)
    ((debug error)
     (progn
       (posframe-delete-all)
       (apply orig-fun args)))))


;;; ----------------------------------------------------------------------

(defun kzk/evil-smart-doc-lookup (prefix)
  "Looks up help for thing at point using the current mode help function, if any.

Call with single prefix will not focus the help window.

Calling with multiple prefix will forward de prefix/4 to the actual help function. The help window will be focused."

  (interactive "p")
  (let ((cur-window (selected-window))
        (current-prefix-arg (when (> prefix 4)
                              ;; Shift-right prefix
                              `(,(/ prefix 4))))
        (p (point)))
    (evil-save-state
      (evil-normal-state)
      (spacemacs/evil-smart-doc-lookup))
    (goto-char p)

    (when (= 4 prefix)
      ;; Calling with sing
      (select-window cur-window))))

;;; ----------------------------------------------------------------------

(defun kzk/patch-display-buffer-override-next-command-action-list (&rest args)
  "Ensures that display-buffer-overriding-action is a list, as it is
expected by display-buffer-override-next-command"

  (unless (listp (car display-buffer-overriding-action))
    (setcar display-buffer-overriding-action (list (car display-buffer-overriding-action)))))

(defun kzk/clean-up-pupo-managed-buffers(&optional remove-buffers)
  "Removes non-live buffers and any byffer in REMOVER-BUFFERS"
  (setq kzk/pupo-managed-buffers
        (--filter (and
                   ;; remove buffers from list from list
                   (not (member it remove-buffers))
                   ;; filter dead buffers
                   (buffer-live-p it))
                  kzk/pupo-managed-buffers)))

;;; ----------------------------------------------------------------------
;;; Popups

(defun kzk/popup-last-no-select (prefix)
  "Pops up the last popup window.

With prefix, selects the window"
  (interactive "P")

  (-when-let* ((prev-wnd (selected-window))
               (buf (car (kzk/clean-up-pupo-managed-buffers)))
               (popup-wnd (display-buffer buf))
               (target-wnd (if prefix popup-wnd prev-wnd)))
    ;; Force select window, since popup rules may cause either
    (select-window target-wnd)))

(defun kzk/popup-last ()
  (interactive)
  (kzk/popup-last-no-select t))

(defun kzk/other-popup (count)
  (interactive "p")

  (-when-let* (
               ;; NO-OP when count=0
               (guard (/= 0 count))
               ;; Find popup buffers and their associated windows
               (popup-buffers (kzk/clean-up-pupo-managed-buffers))
               (popup-windows (--filter (member (window-buffer it) popup-buffers)
                                        (window-list (selected-frame) 'no-minibuf nil)))

               ;; Push the current window to the list if we are not in a popup
               ;; window. This ensure we end up with a sane list in which
               ;; count=1 means go to the next window in the list
               ;; (lst (if (member (window-buffer) popup-buffers)
               ;;          popup-windows
               ;;        (cons (selected-window) popup-windows)))
               ;; (mult (if (< count 0) -1 1))
               ;; (count-modulo (* mult (% count (length popup-windows)))
               ;; (pos (if (< count 0)
               ;;          (- (length lst) count-modulo)
               ;;        count-modulo))
               ;; (window (nth pos lst))))
               )
    ;; Push the current window to the list if we are not in a popup
    ;; window. This ensure we end up with a sane list in which
    ;; count=1 means go to the next window in the list
    (unless (member (window-buffer) popup-buffers)
      (push (selected-window) popup-windows))

    ;; Count modulo the number of windows to find the right position in the
    ;; list
    (setq count (% count (length popup-windows)))

    ;; Negative count -- start from the end
    (when (< count 0)
      (setq count (+ (length popup-windows) count)))

    (-when-let (window (nth count popup-windows))
      (select-window window))))


(defun kzk/consult-switch-to-popup-buffer (prefix)
  "Switchs a buffer managed in a popup window.

With a prefix, includes buffers shown in all frames. "
  (interactive "P")

  ;; We want MRU here, but current buffers should be at the end
  ;; consult--buffer-query does not help much in this aspect, as its sort
  ;; function brings hidden buffers first ...
  (let* ((mru (buffer-list (when (not prefix) (selected-frame))))
         (popup-buffers (kzk/clean-up-pupo-managed-buffers))
         (filtered (--filter (member it popup-buffers)
                             mru))
         (targets (if (member (current-buffer) filtered)
                      (nconc (delq (current-buffer) filtered)
                             (cons (current-buffer) nil))
                    filtered))
         (buffer-names (--map (buffer-name it) targets))
         (source `(
                   :name "Popups"
                   :category buffer
                   :face: consult-buffer
                   :history consult--buffer-history
                   :state ,#'consult--buffer-state
                   :detault t
                   :items ,buffer-names)))
    (consult-buffer (list source))))


(defun kzk/consult-switch-to-popup-buffer-same-purpose ()
  "Switchs a buffer managed in a popup window"
  (interactive)

  (let ((popup-buffers (kzk/clean-up-pupo-managed-buffers)))
    (unless (member (current-buffer) popup-buffers)
      (error "Current buffer is not a popup"))

    (let* ((purpose (purpose-buffer-purpose (current-buffer)))
           (buffers (--filter (eq purpose
                                  (purpose-buffer-purpose it))
                              popup-buffers))
           (source `(
                     :name ,(format "Popups for purpose %s" purpose)
                     :category buffer
                     :face: consult-buffer
                     :history consult--buffer-history
                     :state ,#'consult--buffer-state
                     :detault t
                     :items ,(lambda ()
                               (consult--buffer-query
                                :sort 'visibility
                                :predicate #'(lambda (buf)
                                               (member buf buffers))
                                :as #'buffer-name)
                               ))))
      (consult-buffer (list source)))))

;;; ----------------------------------------------------------------------

(defun kzk/quit-active-minibuffer ()
  "Quits the active minibuffer"

  (interactive)
  (-when-let (w (active-minibuffer-window))
    (save-window-excursion
      (with-selected-window w
        (minibuffer-quit-recursive-edit)))))

;;; ----------------------------------------------------------------------
;;; Other Window management

(defun kzk/other-window-default-cb ()
  (or (when-let* ((win (window-parameter (selected-window) 'kzk/other-window))
                  (live (window-live-p win))
                  (buf (window-buffer win)))
        win)
      (get-mru-window nil nil 'not-this-one-dummy)
      (next-window)               ;fall back to next window
      (next-window nil nil 'visible)))

(defun kzk/esw-set-other-window ()
  "Select a window with ace-window and set it as the \"other
window\" for the current one."
  (interactive)
  (when-let* ((win (save-window-excursion
                     (esw/select-window "Set other window" nil nil)))
              (buf (window-buffer win)))
    (set-window-parameter (selected-window) 'kzk/other-window win)
    (message "Other window is now %S" win)))

(defun kzk/reset-other-window ()
  "Resets the other window -- use default rules"
  (interactive)
  (set-window-parameter (selected-window) 'kzk/other-window nil)
  (message "Using default other-window behaviour"))

(defun kzk/maybe-reset-other-window-parameter ()
  "Avoids keeping references to too many dead windows ..."
  (dolist (win (window-list))
    (when-let (other (window-parameter win 'kzk/other-window))
      (unless (window-live-p other)
        (set-window-parameter win 'kzk/other-window nil)))))

(defun kzk/sync-popper-popups ()
  (setq popper-reference-buffers
        (cl-loop for config-entry in popwin:special-display-config
                 for (pattern . settings) = (popwin:listify config-entry)
                 collect
                 (cond
                  ;; Symbols for major-mode
                  ((symbolp pattern) pattern)
                  ;; A predicate expeting the buffer -- though this is not
                  ;; supported by purpose, therefore not supported by pupo and
                  ;; not used in spacemacs ...
                  ((functionp pattern) pattern)
                  ;; Strings can either be regexp or raw strings
                  ((stringp pattern)
                   (if (plist-get settings :regexp)
                       pattern
                     (regexp-quote pattern)))
                  ;; There should be nothing else , but
                  (t (warn "Invalid type found in popwin:special-display-config: %S" pattern))))))

;;; ----------------------------------------------------------------------
;;; Popup Management (again)
(defvar kzk/last-frame-size nil)
(defun kzk/resize-popups-on-frame-change-hook (frame)
  "Adjust the popup buffers size in case they are too big after a frame resize

The more common use-case is on maximize/unmaximize. Usually
opening a fixed width popup (such as help, fix fixed 75 columns)
results in a big window inside the unmaximized frame. After we
maximize the frame, we do not want that window to occupy all the screen!
"
  (let* ((new-size (cons (frame-width frame) (frame-height frame)))
         (last-size (or kzk/last-frame-size new-size)))
    (setq kzk/last-frame-size new-size)
    (when (not (equal new-size last-size))
      (dolist (w (window-list frame 'not-minibuffer))
        (-when-let* ((buf (window-buffer w))
                     (purpose (purpose-buffer-purpose buf))
                     (cfg (cdr (popwin:match-config buf))
                          )
                     (width (window-width w))
                     (height (window-height w)))
          (pcase purpose
            ((or 'popr 'popl 'coding-assistant)
             (let ((target-width (purpose--normalize-width (plist-get cfg :width))))
               (when (> width target-width)
                 (window-resize w (- target-width width) t))))
            ((or 'popb 'popt)
             (let ((target-height (purpose--normalize-height (plist-get cfg :height))))
               (when (> height target-height)
                 (window-resize w (- target-height height)))))
            (_ nil)))))))
