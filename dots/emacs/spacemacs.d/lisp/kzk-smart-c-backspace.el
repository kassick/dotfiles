;;; Provides a more sane C-<backspace>
;;; C-<backspace> at any word will call clean-aindent to kill or adjust indentation
;;; C-<backspace> at beginning of line will call delete-char-untabify to move the
;;;               cursor to the previous line
;;; Both C-<backspace> and M-<backspace> check if the cursor will erase an opening
;;                     parenthesis and, in that case, call sp-backward-delete-char
;;                       def some_fun(|):, with | as the cursor, when using
;;                     bs-unindent will result first in
;;                       def some_fun:
(defun kzk/backwards-kill-word-or-unindent (arg)
  (interactive "p")
    (if (bolp)
          (backward-delete-char-untabify arg)
        (clean-aindent--bsunindent arg)))

(setq kzk/smart-kill-word-mode-special-stoppers-list
      '((python-mode . (":" "->"))))

(defun kzk/smart-kill-backwards-mode-stopper-regex ()
  (let ((mode-cons (or (assq major-mode kzk/smart-kill-word-mode-special-stoppers-list)
                       (assq t kzk/smart-kill-word-mode-special-stoppers-list))))
    (when mode-cons
      (string-join (cdr mode-cons) "\\|"))
    )
  )

(defun kzk/smart-kill-looking-at-stopper-backwards(&optional greedy)
  (-if-let* ((regexp (kzk/smart-kill-backwards-mode-stopper-regex))
             (regexp (if greedy
                         (concat "\\(" regexp "\\)" "[[:space:]]*")
                       regexp)))
      (looking-back regexp)))

(defun kzk/smart-kill-backwards-stopper-before-word ()
  "If there's a stopper before the last backwards word, return it's ending position"
  (-if-let  (regexp (kzk/smart-kill-backwards-mode-stopper-regex))
      (let* ((prev-word (save-excursion
                          (backward-word)
                          (point)))
             (prev-stopper (save-excursion
                             (search-backward-regexp regexp prev-word t))))
    (when (and prev-stopper (< prev-word prev-stopper))
      (match-end 0))) ))

(defun kzk/around-clean-aindent--bsunindent (fn &rest args)
  "Checks if we're erasing an opening pair or some special sequence
 that should be treated as a word in some language. Adds more
 stops and is very conservative erasing pairs

 Calling bs-unindent with this advice will result in the following sequence:

 def some(other) -> thing: pass|
 def some(other) -> thing: |
 def some(other) -> thing|
 def some(other) -> |
 def some(other)|
 def some(other|)
 def some(|)
 def some|
 def |
 |
"

  (if (and (not (clean-aindent--inside-indentp))
           (bound-and-true-p smartparens-mode))

      ;; def some(other) ->|    or
      ;; def some(other) -> |   or
      (if (kzk/smart-kill-looking-at-stopper-backwards t)
            (progn
              (delete-region (match-beginning 0) (match-end 0)) ; def some(other) |
              (delete-horizontal-space t))                      ; def some(other)|

        ;; def some(other) -> thing|
        (-if-let (last-stopper (kzk/smart-kill-backwards-stopper-before-word))
          (delete-region last-stopper (point)) ; def some(other) ->|

          ;; now handle parens
          (let* ((opening-regex (sp--get-opening-regexp))
                 (closing-regex (sp--get-closing-regexp))
                 (closing-pair-and-spaces-regex (concat closing-regex "[[:space:]]+"))
                 (opening-pair-and-spaces-regex (concat opening-regex "[[:space:]]+")))

            (cond

             ;; some ( thing )     |
             ( (looking-back closing-pair-and-spaces-regex)
               (delete-horizontal-space t))  ; some ( thing )|

             ;; some( thing )|
             ( (looking-back closing-regex)
               (sp-backward-delete-char)     ; some( thing |)
               (delete-horizontal-space t))  ; some( thing|)

             ;; some ( |thing )
             ( (looking-back opening-pair-and-spaces-regex)
               (delete-horizontal-space t))  ; some(|thing)

             ;; some (|  ) thing      or
             ;; some (|) thing        or
             ;; some (|other) thing   or
             ;; some (| } thing       or  (not a pair)
             ;; some (|other } thing      (not a pair)
             ( (looking-back opening-regex)

               (let* ((thing (sp-get-thing t))
                      (thing-contents (when thing (buffer-substring (+ (plist-get thing :beg) (length (plist-get thing :op)))
                                                                    (- (plist-get thing :end) (length (plist-get thing :cl)))))))
                 ;; thing will be some plist if we are inside a valid pair
                 ;; or nil in case of an invalid pair
                 (if (not thing-contents)
                     ;; if not (unbalanced pair) fallback to clean-aindent
                     (apply fn args)

                   ;; inside a valid pair
                   (if (length= (string-trim thing-contents) 0)
                       ;; empty pair
                       (delete-region (plist-get thing :beg)
                                      (plist-get thing :end)) ; some | thing
                     ;; non-empty pair
                     (left-char))                             ; some |(other) thing

                   ;; either way, clean horizontal space to the left
                   (delete-horizontal-space t)))  ; some| thing         or
                                        ; some|other thing
               )

             ( t (apply fn args)) )) ))

    ;; either at indent or no smartparens
    (apply fn args)))

(advice-add 'clean-aindent--bsunindent :around #'kzk/around-clean-aindent--bsunindent)
(with-eval-after-load 'clean-aindent-mode
  (general-define-key :keymaps 'clean-aindent-mode--keymap
                    "C-<backspace>" 'kzk/backwards-kill-word-or-unindent)
  )

(provide 'kzk-smart-c-backspace)
