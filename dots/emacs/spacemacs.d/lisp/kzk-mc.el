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

(defvar kzk/evil-mc-incompatible-minor-modes
  '(smartparens-mode))

(with-eval-after-load 'evil-mc
  ;;; Include my custom movement to MC

  (setq evil-mc-custom-known-commands
        (if (boundp 'evil-mc-custom-known-commands)
            (append evil-mc-custom-known-commands kzk/evil-mc-custom-known-commands)
          kzk/evil-mc-custom-known-commands))

  ;; (setq evil-mc-incompatible-minor-modes (append evil-mc-incompatible-minor-modes kzk/evil-mc-incompatible-minor-modes))

  ;; (after! (:and evil-mc smartparens)
  (dolist (sp-command '(sp-up-sexp
                        sp-copy-sexp
                        sp-down-sexp
                        sp-join-sexp
                        sp-kill-sexp
                        sp-next-sexp
                        sp-split-sexp
                        sp-wrap-curly
                        sp-wrap-round
                        sp-raise-sexp
                        sp-clone-sexp
                        sp-wrap-square
                        sp-splice-sexp
                        sp-end-of-sexp
                        sp-forward-sexp
                        sp-backward-sexp
                        sp-convolute-sexp
                        sp-transpose-sexp
                        sp-kill-whole-line
                        sp-beginning-of-sexp
                        sp-forward-barf-sexp
                        sp-forward-slurp-sexp
                        sp-backward-barf-sexp
                        sp-backward-slurp-sexp
                        sp-splice-sexp-killing-forward
                        sp-splice-sexp-killing-backward))
    (add-to-list
     'evil-mc-custom-known-commands
     `(,sp-command
       (:default . evil-mc-execute-call))))

  )



(with-eval-after-load 'evil
 (require 'evil-multiedit)
 (evil-multiedit-default-keybinds)
 )

(provide 'kzk-mc)
