(defconst kzk-magit-packages
  '(general
    magit))

(defun kzk-magit/post-init-general ())

(defun kzk-magit/post-init-magit ()
  (add-hook 'magit-process-mode-hook 'goto-address-mode)
  (general-define-key :keymaps 'global
                      "C-x g" 'magit-status
                      "C-x M-g"  'magit-dispatch)
  )
