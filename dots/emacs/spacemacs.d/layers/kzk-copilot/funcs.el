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

Called with a prefix, resets the context buffer list before opening"
  (interactive "P")

  (require 'copilot-chat)
  (let ((buf (current-buffer)))
    (when prefix (copilot-chat--clear-buffers))
    (copilot-chat--add-buffer buf)
    (copilot-chat-display)))

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
    (when (string-match-p "^\*Copilot-chat.*\*$"
                          (buffer-name (window-buffer window)))
      (bury-buffer (window-buffer window))
      (delete-window window))))

(defun kzk/copilot-pop-to-chat-buffer ()
  (interactive)
  (-let (((chat-buffer prompt-buffer) (copilot-chat--prepare-buffers)))
    (unless (get-buffer-window prompt-buffer)
      (copilot-chat-display))

    (switch-to-buffer prompt-buffer)))
