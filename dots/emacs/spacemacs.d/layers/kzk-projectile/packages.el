(defconst kzk-projectile-packages '(projectile))

(defun kzk-projectile/post-init-projectile ()
  (spacemacs/set-leader-keys "ps" 'projectile-save-project-buffers)

  (with-eval-after-load 'projectile
    (advice-add 'projectile-find-file
                :around (lambda (fn &rest args)
                          ;; When projectile-find-file is called from the
                          ;; projectile-completing-read :action parameter,
                          ;; this-command is 'vertico-exit.

                          ;; Marginalia uses this-command to check which kind
                          ;; of item is present in the completion by looking
                          ;; up against marginalia-command-categories.

                          ;; Calling projectile-find-file from lisp does not
                          ;; update this-command, so we need to take a hint
                          ;; from embark-become and update it somewhere ...

                          (let ((this-command 'projectile-find-file))
                            (apply fn args))))))
