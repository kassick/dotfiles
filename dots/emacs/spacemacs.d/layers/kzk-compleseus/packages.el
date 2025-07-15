(defconst kzk-compleseus-packages
  `(vertico
    embark
    consult
    compile-multi
    consult-compile-multi
    compile-multi-embark
    compile-multi-all-the-icons
    window-purpose))

(defun kzk-compleseus/post-init-vertico ()
  (with-eval-after-load 'vertico
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
  ;;   (setq orderless-component-separator "[ &-]")
  ;;   (setq orderless-matching-styles '(orderless-regexp orderless-literal orderless-prefixes))
  ;;   )

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
  ;; This was missing from upstream...
  (setq xref-show-definitions-function 'consult-xref)

  ;; Override some spacemacs functions to use prefix to avoid using the input

  ;; First punch some holes in the keymap to get rid of which key replacements ...
  (spacemacs/set-leader-keys
    "sa" nil  ;; ag is not used
    "st" nil  ;; pt is not used
    "sw" nil  ;; search the web is provided only by helm
    )

  (spacemacs/set-leader-keys
    "st" '("Search project TODOs" . kzk/consult-project-todos)

    ;; Add some consult commands in their proper places
    "sgg" #'consult-grep
    "srr" #'consult-ripgrep

    ;; Consult Project Buffers is more powerful than projectile-buffers
    "pb" '("Project Buffers" . consult-project-buffer)
    ))

(defun kzk-compleseus/init-compile-multi ()
  (use-package compile-multi
    :ensure t
    :config
    (setq compile-multi-default-directory #'projectile-project-root)

    (push `((file-exists-p "Makefile")
            ,#'kzk/compile-multi-parse-makefile-rules)
          compile-multi-config)

    (push `(emacs-lisp-mode
            ("emacs:bytecompile" . ,(lambda ()
                                      (byte-compile-file (buffer-file-name)))))
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

(defun kzk-compleseus/init-compile-multi-all-the-icons ()
  (use-package compile-multi-all-the-icons
    :ensure t
    :after compile-multi
    :demand t))

(defun kzk-compleseus/post-init-window-purpose ()

  (spacemacs/set-leader-keys "rb" '("Switch to Buffer with Same Purpose" . kzk/consult-buffer-with-purpose)))

(kzk/after-init

 ;; Sane defaults ...
 (general-define-key :keymaps 'vertico-map
                     "<next>" 'vertico-scroll-up
                     "<prior>" 'vertico-scroll-down
                     "M-q" 'vertico-quick-insert
                     "C-q" 'vertico-quick-exit
                     "RET" #'vertico-directory-enter
                     "DEL" #'vertico-directory-delete-char
                     "M-DEL" #'vertico-directory-delete-word)

 (general-define-key :keymaps 'help-map
                     "B" #'kzk/consult-descbinds)

 (spacemacs/set-leader-keys "?" #'kzk/consult-descbinds)

 (spacemacs/set-leader-keys
   "p/" #'kzk/projectile-consult-fd-find
   "s/" #'kzk/consult-fd-find-here
   )

 (with-eval-after-load 'consult
   (require 'embark-consult)
   (general-define-key :keymaps 'embark-consult-search-map
                       "/" #'consult-fd)

   (setf (alist-get '(file . consult-fd) embark-default-action-overrides
                    nil nil #'equal)
         #'find-file)

   (cl-pushnew #'consult-fd embark-multitarget-actions)
   (cl-pushnew #'embark-consult--async-search-dwim
               (alist-get #'consult-fd embark-around-action-hooks)))

 (all-the-icons-completion-mode +1)

 )
