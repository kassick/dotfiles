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

(defun kzk/around-clean-aindent--bsunindent (fn &rest args)
  "Checks if we're erasing an openin parenthesis and call sp-backward-delete-char instead"

  (if (and (not (clean-aindent--inside-indentp))
           (bound-and-true-p smartparens-mode))

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
                 (thing-contents (when thing (buffer-substring (+ (plist-get thing :beg) 1)
                                                               (- (plist-get thing :end) 1)))))
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

        ( t (apply fn args)) ))

    ;; either at indent or no smartparens
    (apply fn args)))

;; (defun kzk/around-clean-aindent--bsunindent (fn &rest args)
;;   "Checks if we're erasing an openin parenthesis and call sp-backward-delete-char instead"

;;   (if (bound-and-true-p smartparens-mode)
;;       (cond

;;        ;; inside parenthesis with some space to the left, delete all space inside the parenthesis
;;        ( (and (looking-back "(\s+")
;;               (looking-at "\s*)") )
;;          (message "inside parenthesis, zapping ")
;;          (ignore-errors (zap-up-to-char 1 ?\) ))
;;          (ignore-errors (zap-up-to-char -1 ?\( )) )

;;        ;; cursor outsize of a close parenthesis plus space; erase up to parenthesis
;;        ( (looking-back ")\s+")
;;          (zap-up-to-char -1 ?\) ) )

;;        ;; cursor right outside of closing parenthesis; cal sp-backward-delete-char
;;        ( (char-equal (char-before) ?\) ) (call-interactively 'sp-backward-delete-char) )

;;        ;; cursor to the right of an open parenthesis; smart-parens delete it
;;        ( (char-equal (char-before) ?\( )  (call-interactively 'sp-backward-delete-char) )

;;        ;; default case: call clean-aindent--bsunindent
;;        ( t (apply fn args)) )

;;     ;; if not smartparens
;;     (apply fn args)
;;     )

;;   ;; (if (and (bound-and-true-p smartparens-mode)
;;   ;;          (char-equal (char-before) ?\( ))
;;   ;;     (call-interactively 'sp-backward-delete-char)
;;   ;;   (apply fn args))
;;   )

(advice-add 'clean-aindent--bsunindent :around #'kzk/around-clean-aindent--bsunindent)
(with-eval-after-load 'clean-aindent-mode
  (general-define-key :keymaps 'clean-aindent-mode--keymap
                    "C-<backspace>" 'kzk/backwards-kill-word-or-unindent)
  )

(provide 'kzk-smart-c-backspace)
