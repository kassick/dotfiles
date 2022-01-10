;;; Provides a more sane C-<backspace>
;;; C-<backspace> at any word will call clean-aindent to kill or adjust indentation
;;; C-<backspace> at beginning of line will call delete-char-untabify to move the
;;;               cursor to the previous line

(defun kzk/backwards-kill-word-or-unindent (arg)
  (interactive "p")
  (if (bolp)
      (backward-delete-char-untabify arg)
    (clean-aindent--bsunindent arg)))

(with-eval-after-load 'clean-aindent-mode
  (general-define-key :keymaps 'clean-aindent-mode--keymap
                      "C-<backspace>" 'kzk/backwards-kill-word-or-unindent))

(provide 'kzk-smart-c-backspace)
