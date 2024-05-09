(defconst kzk-compleseus-packages
  '(vertico
    ))

(defun kzk-compleseus/post-init-vertico ()
  (with-eval-after-load 'vertico
    (require 'vertico-multiform)
    (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
    (vertico-multiform-mode)

    (require 'vertico-mouse)
    (vertico-mouse-mode)

    (message "Setting up vertico")
    (setq vertico-cycle t))

  (with-eval-after-load 'orderless
    (setq orderless-matching-styles '(orderless-regexp orderless-literal orderless-prefixes)))

  )

(kzk/after-init

 ;; Sane defaults ...
 (general-define-key :keymaps 'vertico-map
                     "<next>" 'vertico-scroll-up
                     "<prior>" 'vertico-scroll-down
                     "M-q" 'vertico-quick-insert
                     "C-q" 'vertico-quick-exit
                     )
 )
