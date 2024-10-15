(defun kzk/copilot-add-current-buffer ()
  "Adds the current buffer to copilot chat context"
  (interactive)
  (require 'copilot-chat)
  (copilot-chat-add-current-buffer))

(defun kzk//copilot-add-buffers (clear buffers)
  (require 'copilot-chat)
  (when clear
    (copilot-chat--clear-buffers))
  (let ((n 0))
    (dolist (buf buffers)
      (unless (or (string-match-p "^\*.*\*$" (buffer-name buf))
                  (string-prefix-p " " (buffer-name buf)))
        (copilot-chat--add-buffer buf)
        (setq n (1+ n))))
    n))

(defun kzk/copilot-add-projectile-buffers (prefix)
  "Adds the project buffers to the chat context"
  (interactive "P")

  (message "Added %d buffers of project %s"
           (kzk//copilot-add-buffers prefix (projectile-project-buffers))
           (projectile-project-name)))


(defun kzk/copilot-add-layout-buffers (prefix)
  "Adds the layout buffers to the chat context"

  (interactive "P")

  (message "Added %d buffers present in current layout"
           (kzk//copilot-add-buffers prefix (persp-buffer-list))))


(defun kzk/copilot-add-frame-buffers (prefix)
  "Adds the buffers currently visible in the frame to the chat context"

  (interactive "P")

  (message "Added %d buffers displayed in current frame"
           (kzk//copilot-add-buffers prefix
                                     (--map (window-buffer it)
                                            (window-list nil 'no-minibuf)))))

(defun kzk/copilot-chat-display (prefix)
  "Opens the Copilot chat window, adding the current buffer to the context.

Called with a prefix, resets the context buffer list before opening.
Calling with double prefix, will reset the chat"
  (interactive "p")

  (require 'copilot-chat)
  (let ((buf (current-buffer)))

    ;; Explicit reset before doing anything, avoid it resetting later on
    ;; target-fn and ignoring the added buffers
    (unless (copilot-chat--ready-p)
      (copilot-chat-reset))

    (when (>= prefix 4) (copilot-chat--clear-buffers))
    (when (>= prefix 16) (copilot-chat-reset))

    (copilot-chat--add-buffer buf)
    (copilot-chat-display)))

(defun kzk/copilot-chat-shell-maker-display (prefix)
  "Opens the Copilot chat window, adding the current buffer to the context.

Called with a prefix, resets the context buffer list before opening.
Calling with double prefix, will reset the chat"
  (interactive "p")

  (require 'copilot-chat)
  (interactive)

  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))

  (when (>= prefix 4) (copilot-chat--clear-buffers))
  (when (>= prefix 16) (copilot-chat-reset))

  (let ((buffer (get-buffer copilot-chat--buffer))
        (tempb (get-buffer-create copilot-chat--shell-maker-temp-buffer))
        (inhibit-read-only t))
    (unless buffer
      (setq buffer (copilot-chat--shell)))
    (with-current-buffer tempb
      (markdown-view-mode))
    (display-buffer buffer)))

(defun kzk/copilot-chat-add-buffer (buffer)
  "Adds buffer to the copilot chat context"
  (interactive "b")
  (require 'copilot-chat)
  (let ((buf (get-buffer buffer)))
    (copilot-chat--add-buffer buf)
    (message "Buffer %s added to Copilot Chat Context" (buffer-name buf))))

;; (defun kzk/shell-maker-display-buffer (buffer &optional no-record)
;;   (message "switching to %S" buffer)
;;   (let ((cur-f (selected-frame))
;;         (target-frame (-if-let (copilot-frame
;;                                 (--find (string-equal (frame-parameter it 'name)
;;                                                       "copilot chat")
;;                                         (frame-list)))
;;                           copilot-frame
;;                         (make-frame '((name . "copilot chat"))))))
;;       (select-frame target-frame)
;;       (pop-to-buffer-same-window buffer)))

(defun kzk/copilot-chat-delete-windows ()
  (interactive)

  (dolist (window (window-list))
    (when (string-match-p "^\*[Cc]opilot-chat.*\*$"
                          (buffer-name (window-buffer window)))
      (bury-buffer (window-buffer window))
      (delete-window window))))

(defun kzk/copilot-pop-to-chat-buffer ()
  (interactive)
  (-let (((chat-buffer prompt-buffer) (copilot-chat--prepare-buffers)))
    (unless (get-buffer-window prompt-buffer)
      (copilot-chat-display))

    (switch-to-buffer prompt-buffer)))


(defmacro kzk//copilot-chat-call-with-add-buffer-fn (context-name add-buffers-fn target-fn)
  `(defun ,(intern (concat "kzk/" context-name "-" (symbol-name target-fn)) ) (prefix)
     ,(format "Calls %s in the context of the current %s buffers"
              (symbol-name target-fn)
              context-name)
     (interactive "P")


     (require 'copilot-chat)

     ;; Explicit reset before doing anything, avoid it resetting later on
     ;; target-fn and ignoring the added buffers
     (unless (copilot-chat--ready-p)
       (copilot-chat-reset))

     (when prefix (copilot-chat--clear-buffers))

     (apply (symbol-function ',add-buffers-fn) (list prefix))
     (call-interactively (symbol-function ',target-fn))))

(defun kzk//make-context-map (name add-buffers-fn)
  (let ((map (make-sparse-keymap))
        (bindings `(("c" "Copilot Chat" copilot-chat-display)
                    ("a" "Copilot Ask and Insert" copilot-chat-ask-and-insert)
                    ("e" "Copilot Explain"  copilot-chat-explain)
                    ("r" "Copilot Review"  copilot-chat-review)
                    ("h" "Copilot Write Documentation"  copilot-chat-doc)
                    ("f" "Copilot Fix"  copilot-chat-fix)
                    ("o" "Copilot Optimize"  copilot-chat-optimize)
                    ("t" "Copilot Tests"  copilot-chat-test)))
        )
    (dolist (binding bindings)
      (-let* (((key doc fn) binding)
              ;; Workaround bizarre macro expansion issue -- expecing sequencep because of concatenating name
              ;; see https://emacs.stackexchange.com/questions/60848/how-to-properly-generate-a-concatd-function-name-with-defmacro
              (target-fn (eval `(kzk//copilot-chat-call-with-add-buffer-fn ,name ,add-buffers-fn ,fn))))
        (define-key map (kbd key) (cons doc target-fn))))
    (define-key map
                (kbd "+")
                `(,(format "Add %s buffers to Copilot Chat context" name) . ,add-buffers-fn))

    map))

(defun kzk/copilot-complete (prefix)
  "Complete with Copilot. When called with prefix, display the completion panel"
  (interactive "P")

  (if prefix
      (copilot-panel-complete)
    (copilot-complete)))

(defun kzk/copilot--handle-notifications-advice (_ method msg)
  (pcase method
    ('PanelSolution
     (with-current-buffer "*copilot-panel*"
       (deactivate-mark)
       (goto-char (point-min))))))
