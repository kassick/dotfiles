;;; package --- Summary

;;; This mode provides a smart home/end keys, moving the cursor to the nearest logical point.

;;; - home: places the cursor in one of the following points, by priority:
;;;   - beginning of visual line
;;;   - indent
;;;   - start of line
;;; - end: places the cursor by priority in:
;;;   - end of visual line

;;; Commentary:

;;; Code:

(defmacro kzk/execute-and-get-point-or-bail (&rest FORMS)
  "Return the point after executing FORMS, or the value of bail."

  `(let ((opoint (point))
         (bail-value (if (boundp 'bail) bail -1)))
     ;; (or (boundp 'bail) (setq bail -1))
     ;; (message "bailing to %S" bail)
     (save-excursion
       (eval ,@FORMS)
       ;;(message "moved to %S from %S" (point) opoint)
       (if (= (point) opoint)
           bail-value
         (point)))))

(defun kzk/beginning-of-visual-line-or-indent (&optional n)
  "Go to the logical beginning of the N'th line.

  Move point to closest point among:
  - beginning of current visual line
  - indent
  - first character"
  (interactive "^p")

  (or n (setq n 1))
  (when (/= n 1)
    (let ((line-move-visual t))
      (line-move (1- n) t)))

  (let* (
         (point-field
          (kzk/execute-and-get-point-or-bail (field-beginning)))

         (point-starting-of-visual-line
          (kzk/execute-and-get-point-or-bail (when (bound-and-true-p visual-line-mode)
                                               (vertical-motion 0))))

         (point-indent
          ;; back-to-indentation moves to the next char after indentation. In a
          ;; blank line, this will always move to the next char, resultin in
          ;; this movement never bailing
          (kzk/execute-and-get-point-or-bail (when (not (looking-back "^[[:space:]]\+" 0))
                                               (back-to-indentation))))

         (point-starting-of-line
          (kzk/execute-and-get-point-or-bail (move-beginning-of-line 1)))

         (target (max point-field point-starting-of-visual-line point-indent point-starting-of-line)))

    ;; (message "cur: %S field: %S visual: %S indent: %S line: %S" (point) point-field point-starting-of-visual-line point-indent point-starting-of-line)

    (when (/= target -1)
      (goto-char target))) )

(defun kzk/end-of-visual-line-or-eol (&optional n)
  "Move cursor to the logical end of the N'thline.

  Move point to the closest point among:
  - end of visual line
  - end of line"

  (interactive "^p")
  (or n (setq n 1))

  (when (/= n 1)
    (let ((line-move-visual t))
      (line-move (1- n) t)))

  (let* ((bail (1+ (point-max)))

         (point-end-of-field
          (kzk/execute-and-get-point-or-bail (field-end)))

         (point-end-of-visual-line
          (kzk/execute-and-get-point-or-bail (when (bound-and-true-p visual-line-mode)
                                                    (vertical-motion (cons (window-width) 0)))))

         (point-end-of-line-non-blank
          (kzk/execute-and-get-point-or-bail (evil-last-non-blank)
                                             (right-char 1)
                                             ))

         (point-end-of-line
          (kzk/execute-and-get-point-or-bail (end-of-line 1)))

         (target (min point-end-of-field point-end-of-visual-line
                      point-end-of-line-non-blank
                      point-end-of-line)))

    ;; (message "target: %S field: %S visual: %S target line-non-blank: %S eol: %S" target point-end-of-field point-end-of-visual-line point-end-of-line-non-blank point-end-of-line)
    (when (/= target bail)
      (goto-char target))
    ))


(defvar kzk-smart-home-end-mode-keymap (make-sparse-keymap)
  "Keymap for kzk/smart-home-end-mode.")


(defgroup kzk-smart-home-end ()
  "Smart home end mode."
  :group 'editing
  :prefix "kzk-smart-home-end-mode")

(defcustom kzk-smart-home-end-mode-ignored-major-modes
  '(vterm-mode term-mode eshelm-mode helm-mode minibuffer-mode minibuffer-inactive-mode)
  "List of modes where the global mode won't be automatically initialized."
  :group 'kzk-smart-home-end
  :type '(repeat symbol))


(define-key kzk-smart-home-end-mode-keymap
            (kbd "<home>") 'kzk/beginning-of-visual-line-or-indent)
(define-key kzk-smart-home-end-mode-keymap
            (kbd "<end>") 'kzk/end-of-visual-line-or-eol)

(with-eval-after-load 'evil
  (require 'evil-commands)
  (evil-define-key '(normal visual motion insert hybrid replace iedit-insert) kzk-smart-home-end-mode-keymap
                      (kbd "<home>") 'kzk/beginning-of-visual-line-or-indent
                      (kbd "<end>") 'kzk/end-of-visual-line-or-eol))


(define-minor-mode kzk-smart-home-end-mode
  "Toggle kzk/smart-home-end-mode."

  :init-value nil
  :lighter (" 🏠")
  :group 'kzk-smart-home-end
  :keymap kzk-smart-home-end-mode-keymap
  )


(defun turn-on-kzk-smart-home-end-mode ()
  "Turn on the smart home end mode."
  (interactive)
  (unless (or (member major-mode kzk-smart-home-end-mode-ignored-major-modes)
              (and (not (derived-mode-p 'comint-mode))
                   (eq (get major-mode 'mode-class) 'special)))
    (kzk-smart-home-end-mode t)))



(define-global-minor-mode kzk-smart-home-end-global-mode
  kzk-smart-home-end-mode
  turn-on-kzk-smart-home-end-mode)

(provide 'kzk-smart-home-end)
;;; kzk-smart-home-end.el ends here
