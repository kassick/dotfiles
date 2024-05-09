(defconst kzk-compleseus-packages
  '(vertico
    ))

(defun kzk-compleseus/post-init-vertico ()
  (with-eval-after-load 'vertico
    (require 'vertico-multiform)
    (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
    (vertico-multiform-mode)

    (message "Setting up vertico")
    (setq vertico-cycle t))


  )
