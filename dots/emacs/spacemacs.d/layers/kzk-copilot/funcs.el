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
      (delete-window window))))

(defun kzk/copilot-pop-to-chat-buffer ()
  (interactive)
  (-let (((chat-buffer prompt-buffer) (copilot-chat--prepare-buffers)))
    (unless (get-buffer-window prompt-buffer)
      (copilot-chat-display))

    (switch-to-buffer prompt-buffer)))
