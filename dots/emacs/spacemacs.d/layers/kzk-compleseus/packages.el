(defconst kzk-compleseus-packages
  '(vertico
    embark
    consult
    compile-multi
    consult-compile-multi
    compile-multi-embark
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

(defun kzk-compleseus/post-init-consult ()
  ;; Override some spacemacs functions to use prefix to avoid using the input

  (spacemacs/set-leader-keys
    "sS" #'kzk/spacemacs/search-line
    "sB" #'kzk/spacemacs/search-line-multi-project
    "s C-b" #'kzk/spacemacs/search-line-multi-all
    "sf" #'kzk/spacemacs/compleseus-search-auto
    "sd" #'kzk/spacemacs/compleseus-search-dir
    "sp" #'kzk/spacemacs/compleseus-search-projectile
    "*"  #'kzk/spacemacs/compleseus-search-projectile
    )
  )

(defun kzk-compleseus/init-compile-multi ()
  (use-package compile-multi
    :ensure t
    :config
    (setq compile-multi-default-directory #'projectile-project-root)

    (push `((file-exists-p "Makefile")
            ,#'kzk/compile-multi-parse-makefile-rules)
          compile-multi-config)

    :init

    (spacemacs/set-leader-keys "cc" 'compile-multi)

    )

  )

(defun kzk-compleseus/init-consult-compile-multi ()
  (use-package consult-compile-multi
    :ensure t
    :after compile-multi
    :demand t
    :config (consult-compile-multi-mode)))

(defun kzk-compleseus/init-compile-multi-embark ()
  (use-package compile-multi-embark
    :ensure t
    :after embark
    :after compile-multi
    :demand t
    :config (compile-multi-embark-mode +1)))

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

                     :keymaps 'help-map
                     "B" #'kzk/consult-descbinds
                     )

 (spacemacs/set-leader-keys "?" #'kzk/consult-descbinds)

 )
