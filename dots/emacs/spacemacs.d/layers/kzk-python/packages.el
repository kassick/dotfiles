(defconst kzk-python-packages
  '(smartparens))

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
