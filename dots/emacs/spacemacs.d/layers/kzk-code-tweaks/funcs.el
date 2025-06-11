(defun kzk/projectile-pop-shell ()
  (interactive)

  (cl-letf (((symbol-function 'spacemacs//current-layout-name)
             #'projectile-project-name))
    (spacemacs/projectile-shell-pop)))
