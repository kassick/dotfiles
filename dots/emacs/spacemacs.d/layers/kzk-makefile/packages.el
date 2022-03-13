(defconst kzk-makefile-packages '(make-mode))

(defun kzk-makefile/init-make-mode ()
  (add-hook 'makefile-mode-hook
            (lambda ()
              (setq-local indent-line-function 'indent-relative)))
  )
