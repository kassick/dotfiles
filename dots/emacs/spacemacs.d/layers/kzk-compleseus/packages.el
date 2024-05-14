(defconst kzk-compleseus-packages
  '(vertico
    embark
    ))

(defun kzk-compleseus/post-init-vertico ()
  (with-eval-after-load 'vertico
    (message "Setting up vertico")

    ;; compleseus setq this in compleseus/init-vertico, so we can not simply
    ;; setq or set default values in config.el -- must do it manually here :/
    (setq vertico-cycle t)

    ;; Display embark actions as grid
    ;; (require 'vertico-multiform)
    ;; (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
    ;; (vertico-multiform-mode)

    ;; Enter on directories browses to them ; m-del is the same as C-h

    ;; Scrolling is sometimes useful..
    (require 'vertico-mouse)
    (vertico-mouse-mode)

    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

    )

  ;; (with-eval-after-load 'orderless
  ;;   (setq orderless-matching-styles '(orderless-regexp orderless-literal orderless-prefixes)))

  )

(defun kzk-compleseus/post-init-embark ()
  ;; DO NOT USE EMBARK FOR KEY DISPATCH
  ;;
  ;; Spacemacs nested keymaps do are not keymaps (according do keymapp
  ;; (!!!!!?)), so embark does not unnest them correctly. It also does not
  ;; work on recursive maps (because reasons, SPC k SPC goes back to the root
  ;; spacemacs map ...)
  ;;
  ;; Instead, use a half-baked consult-descbinds (adapted from helm-descbinds)
  (setq prefix-help-command #'which-key-C-h-dispatch
        which-key-use-C-h-commands t)
  (advice-add 'describe-bindings :override #'kzk/consult-descbinds)
)

(kzk/after-init

 ;; Sane defaults ...
 (general-define-key :keymaps 'vertico-map
                     "<next>" 'vertico-scroll-up
                     "<prior>" 'vertico-scroll-down
                     "M-q" 'vertico-quick-insert
                     "C-q" 'vertico-quick-exit
                     "RET" #'vertico-directory-enter
                     "DEL" #'vertico-directory-delete-char
                     "M-DEL" #'vertico-directory-delete-word
                     )

    (spacemacs/set-leader-keys "?" #'kzk/consult-descbinds)
    (define-key (kbd "C-h B") #'kzk/consult-descbinds)


 )
