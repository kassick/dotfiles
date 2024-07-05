(defconst kzk-tree-sitter-packages
  '(tree-sitter-langs
    ts-fold))

(defun kzk-tree-sitter/post-init-tree-sitter-langs ()
  ;; Force call here to make sure this happens in the startup process. If left
  ;; only to be set by custom, it may not be correctly set up if a markdown
  ;; file is the first file opened -- the hook happens after the tree sitter
  ;; has been set up and loaded for the buffer... i.e. too late :/
  (kzk/tree-sitter-remove-verboten-languages)
  )

(defun kzk-tree-sitter/post-init-ts-fold ()
  (spacemacs/set-leader-keys "T C-f" '("Toggle Fold Indicators" . ts-fold-indicators-mode))
  (setq ts-fold-indicators-priority 0))
