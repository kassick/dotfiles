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

(defun kzk/execute-and-get-point-or-bail-internal (bail FORMS)
  "Return the point after executing FORMS, or the value of bail."

  (let ((opoint (point)))
     (save-excursion
       (dolist (form FORMS)
         (eval form))

       (if (= (point) opoint)
           bail
         (point)))))


(defmacro kzk/execute-and-get-point-or-bail (&rest FORMS)
  "Return the point after executing FORMS, or the value of bail."

  ;; (message "macro called with forms %S" FORMS)
  (let ((res `(let ((bail-value (if (boundp 'bail) bail -1)))
     (kzk/execute-and-get-point-or-bail-internal bail-value ',FORMS))))
    ;; (message "built form %S" res)
    res
    ))

;; (defmacro kzk/execute-and-get-point-or-bail (&rest FORMS)
;;   "Return the point after executing FORMS, or the value of bail."

;;   (message "called with forms %S" FORMS)
;;   `(let ((opoint (point))
;;          (bail-value (if (boundp 'bail) bail -1)))
;;      ;; (or (boundp 'bail) (setq bail -1))
;;      ;; (message "bailing to %S" bail)
;;      (save-excursion
;;        (eval ,@FORMS)
;;        ;;(message "moved to %S from %S" (point) opoint)
;;        (if (= (point) opoint)
;;            bail-value
;;          (point)))))


(defmacro kzk/point-at-p (&rest FORMS)
  `(let ((opoint (point))
          (target-point (save-excursion
                          (eval ,@FORMS)
                          (point))))
     ;;(message "opoint=%S target=%S" opoint target-point)
     (= opoint target-point)))


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

  (cond
   ;; Alternate between start of line and indent

   ;; At the beginning of line
   ((kzk/point-at-p (beginning-of-line))
    ;; go back to indentation
    (back-to-indentation))

   ;; At the indentation
   ((kzk/point-at-p
     (back-to-indentation)
     ;; When at an empty line, back-to-indentation will leave the cursor past
     ;; the last indent char. In that case, go back one char
     (when (looking-at "$") (left-char)))
    ; back to beginning of line
    (beginning-of-line))

   ;; Otherwise, find the closest target
   (t
    (let* (
         (point-field
          (kzk/execute-and-get-point-or-bail (field-beginning)))

         (point-starting-of-visual-line
          (kzk/execute-and-get-point-or-bail (when (bound-and-true-p visual-line-mode)
                                               (vertical-motion 0))))

         (point-indent
          (kzk/execute-and-get-point-or-bail (back-to-indentation)))

         (point-starting-of-line
          (kzk/execute-and-get-point-or-bail (move-beginning-of-line 1)))

         (target (max point-field point-starting-of-visual-line point-indent point-starting-of-line)))

    ;; (message "cur: %S field: %S visual: %S indent: %S line: %S" (point) point-field point-starting-of-visual-line point-indent point-starting-of-line)

    (if (= (point) point-starting-of-line)
        (back-to-indentation)
      (when (/= target -1)
        (goto-char target))) )
    )
   )


  )

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
    (if (/= (point) point-end-of-line)
        (when (/= target bail)
          (goto-char target))
      ;; else
      (evil-last-non-blank)
      (right-char 1))
    ))


(defgroup kzk-smart-home-end ()
  "Smart home end mode."
  :group 'editing
  :prefix "kzk-smart-home-end-mode")

(defcustom kzk-smart-home-end-mode-ignored-major-modes
  '(vterm-mode term-mode eshelm-mode helm-mode minibuffer-mode minibuffer-inactive-mode vertico-mode)
  "List of modes where the global mode won't be automatically initialized."
  :group 'kzk-smart-home-end
  :type '(repeat symbol))


(define-minor-mode kzk-smart-home-end-mode
  "Toggle kzk/smart-home-end-mode."

  :init-value nil
  :lighter (" 🏠")
  :group 'kzk-smart-home-end
  ;;:keymap kzk-smart-home-end-mode-keymap
  )

(with-eval-after-load 'evil
  (evil-define-minor-mode-key
    '(normal visual motion insert hybrid replace iedit-insert emacs operator)
    'kzk-smart-home-end-mode
    (kbd "<home>") 'kzk/beginning-of-visual-line-or-indent
    (kbd "<end>") 'kzk/end-of-visual-line-or-eol))
  ;;(evil-define-minor-mode-key '(normal visual motion insert hybrid replace iedit-insert) 'kzk-smart-home-end-mode (kbd "<end>") 'kzk/end-of-visual-line-or-eol))
;; (defvar kzk-smart-home-end-mode-keymap (make-sparse-keymap)
;;   "Keymap for kzk/smart-home-end-mode.")
;;
;; (define-key kzk-smart-home-end-mode-keymap
;;             (kbd "<home>") 'kzk/beginning-of-visual-line-or-indent)
;; (define-key kzk-smart-home-end-mode-keymap
;;             (kbd "<end>") 'kzk/end-of-visual-line-or-eol)
;; (with-eval-after-load 'evil
;;   (require 'evil-commands)
;;   (evil-define-key '(normal visual motion insert hybrid replace iedit-insert) kzk-smart-home-end-mode-keymap
;;     (kbd "<home>") 'kzk/beginning-of-visual-line-or-indent
;;     (kbd "<end>") 'kzk/end-of-visual-line-or-eol))




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
