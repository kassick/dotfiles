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
  ;; Remove debounce -- preview can be really annoying!
  ;; See spacemacs compleseus/packages.el for the original values
  (consult-customize
   consult-theme
   spacemacs/theme-loader
   :preview-key '("M-." "C-SPC")

   ;; slightly delayed preview upon candidate selection
   ;; one usually wants quick feedback
   consult-buffer
   consult-ripgrep
   consult-git-grep
   consult-grep
   consult-bookmark
   consult-yank-pop
   :preview-key '("M-." "C-SPC"))

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
    "ss" #'kzk/search-in-buffer
    "sS" #'kzk/search-in-buffer-with-input
    "sb" #'kzk/search-in-project-buffers
    "sB" #'kzk/search-in-project-buffers-with-input
    "s C-b" #'kzk/search-in-all-buffers
    "s C-S-b" #'kzk/search-in-all-buffers-with-input
    "sf" #'kzk/search-from-path
    "sF" #'kzk/search-from-path-with-input
    "sd" #'kzk/search-in-current-dir
    "sD" #'kzk/search-in-current-dir-with-input
    "sp" #'kzk/search-in-project
    "sP" #'kzk/search-in-project-with-input
    "st" '("Search project TODOs" . kzk/consult-project-todos)
    "*"  #'kzk/search-in-project-with-input
    "/"  #'kzk/search-in-project

    ;; Add some consult commands in their proper places
    "sgg" #'consult-grep
    "srr" #'consult-ripgrep

    ;; Consult Project Buffers is more powerful than projectile-buffers
    "pb" '("Project Buffers" . consult-project-buffer)
    )



  ;; Customize symbol search
  (spacemacs/transient-state-register-remove-bindings
    'symbol-highlight
    '("b" "/"))
  (spacemacs/transient-state-register-add-bindings 'symbol-highlight
    '(
      ;; replace with my custom project search -- upstream
      ;; uses a function that ignores the current symbol input,
      ;; which looks like a bug
      ("/" kzk/search-in-project-buffers-with-input :exit t)
      ;; Search the current symbol on all the buffers
      ("b" kzk/search-in-all-buffers-with-input :exit t)))
  (setq spacemacs--symbol-highlight-transient-state-doc
        (concat
         spacemacs--symbol-highlight-transient-state-doc
         ;; "[_/_] project" is already there, only need to append the buffers shortcut
         "  [_b_] buffers"))
  )

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

 (with-eval-after-load 'embark-consult
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
