(defconst kzk-python-packages
  '(smartparens))

(defun kzk-python/post-init-smartparens ())

(kzk/after-init
 (spacemacs/set-leader-keys-for-major-mode 'python-mode
   "re" 'kzk/py-refactor-extract-arg))
