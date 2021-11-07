(defvar kzk/evil-mc-custom-known-commands
  '((kzk/beginning-of-visual-line-or-indent . ((:default . evil-mc-execute-default-call-with-count)))
    (kzk/end-of-visual-line-or-eol . ((:default . evil-mc-execute-default-call-with-count)))
    (evil-inner-arg . ((:default . evil-mc-execute-default-call-with-count)
                       (visual . evil-mc-execute-visual-text-object)))
    (evil-outer-arg . ((:default . evil-mc-execute-default-call-with-count)
                       (visual . evil-mc-execute-visual-text-object)))
    )
  )


(with-eval-after-load 'evil-mc
  ;;; Include my custom movement to MC

  (setq evil-mc-custom-known-commands
        (if (boundp 'evil-mc-custom-known-commands)
            (append evil-mc-custom-known-commands kzk/evil-mc-custom-known-commands)
          kzk/evil-mc-custom-known-commands)))

(provide 'kzk-mc)
