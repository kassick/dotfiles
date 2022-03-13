(defconst kzk-projectile-packages '(projectile))

(defun kzk-projectile/post-init-projectile ()
  (spacemacs/set-leader-keys "ps" 'projectile-save-project-buffers))
