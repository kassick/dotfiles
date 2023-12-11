(defconst kzk-tree-sitter-packages
  '(tree-sitter-langs))

(defun kzk-tree-sitter/post-init-tree-sitter-langs ()
  ;; Nothing to do here -- the defcustom already handles calling kzk/tree-sitter-remove-verboten-languages on update
  )
