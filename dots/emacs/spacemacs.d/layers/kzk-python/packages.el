(defconst kzk-python-packages
  '(smartparens
    helm-pydoc
    ;pyvenv
    ))

(custom-set-variables
 '(importmagic-style-configuration-alist '((multiline . backslash) (max_columns . 1200))))

(defun kzk-python/post-init-smartparens ()
  (spacemacs/set-leader-keys-for-major-mode 'python-mode
    "re" 'kzk/py-refactor-extract-arg
    "=y" 'yapfify-region-or-buffer)

  (custom-set-variables
   ;; disable this annoying behaviour, since pressing colon does not jump over
   ;; the included colon
   '(sp-python-insert-colon-in-function-definitions nil)))

(defun kzk-python/post-init-helm-pydoc ()
  ;;; Rewrite helm-pydoc to support current region
  (defun helm-pydoc (start end)
    (interactive "r")
    (let ((initial-input (when (region-active-p)
                           (buffer-substring-no-properties start end))))
      (helm :sources '(helm-pydoc--imported-source helm-pydoc--installed-source)
            :buffer "*helm pydoc*" :history 'helm-pydoc--history
            :input initial-input))))


;; (defun kzk-python/post-init-pyvenv ()
;;   (message "advising spacemacs pyenv")
;;   (advice-add 'spacemacs//pyvenv-mode-set-local-virtualenv :around #'kzk/spacemacs-pyenv-mode-set-local-virtualenv))
