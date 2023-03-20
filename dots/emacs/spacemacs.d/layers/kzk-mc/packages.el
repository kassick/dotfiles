(defconst kzk-mc-packages
  '(evil
    evil-mc))

(defun kzk-mc/post-init-evil-mc ()
  ;;; Include my custom movement to MC
  (setq evil-mc-custom-known-commands
        (if (boundp 'evil-mc-custom-known-commands)
            (append evil-mc-custom-known-commands kzk/evil-mc-custom-known-commands)
          kzk/evil-mc-custom-known-commands))

  ;; (setq evil-mc-incompatible-minor-modes (append evil-mc-incompatible-minor-modes kzk/evil-mc-incompatible-minor-modes))

  ;; permit sp commands in evil-mc mode
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

(defun kzk-mc/init-evil-multiedit ()
  (require 'evil-multiedit)
  (evil-multiedit-default-keybinds)
  )

(defun kzk-mc/post-init-evil ()
  (kzk/after-init
   (define-key evil-ex-search-keymap
               (kbd "C-E") 'kzk/evil-iedit-from-current-ex-search)
   (spacemacs/set-leader-keys "s E" 'kzk/evil-iedit-from-last-ex-search)))
