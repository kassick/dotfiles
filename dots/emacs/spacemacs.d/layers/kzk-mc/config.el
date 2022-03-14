(defvar kzk/evil-mc-custom-known-commands
  '((kzk/beginning-of-visual-line-or-indent . ((:default . evil-mc-execute-default-call-with-count)))
    (kzk/end-of-visual-line-or-eol . ((:default . evil-mc-execute-default-call-with-count)))
    (evil-inner-arg . ((:default . evil-mc-execute-default-call-with-count)
                       (visual . evil-mc-execute-visual-text-object)))
    (evil-outer-arg . ((:default . evil-mc-execute-default-call-with-count)
                       (visual . evil-mc-execute-visual-text-object)))
    ;; (evil-quoted-insert . ((:default . evil-mc-execute-default-call-with-count)))
    )
  )

;; (defvar kzk/evil-mc-incompatible-minor-modes
;;   '(smartparens-mode))
