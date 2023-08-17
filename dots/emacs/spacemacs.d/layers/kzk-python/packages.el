(defconst kzk-python-packages
  '(smartparens
    helm-pydoc))

(custom-set-variables
 '(importmagic-style-configuration-alist '((multiline . backslash) (max_columns . 1200))))

;; (defun kzk-python/post-init-lsp-mode ()
;;   "Spacemacs python layer defines = as a prefix key in spacemacs-python-mode-map, which masks
;;    spacemacs's lsp layer definition of =
;;   "
;;   ;;; DOES NOT WORK -- order of evaluation -- kzk-lsp will define == as format,
;;   ;;; overriding this definition
;;   (message "setting lsp-mode minor mode == from kzk-python")
;;   (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode "==" #'spacemacs/python-format-buffer))

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
