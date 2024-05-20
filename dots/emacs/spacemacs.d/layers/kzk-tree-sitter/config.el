(defcustom kzk/tree-sitter-verboten-modes
  '()
  "List of modes that should not use tree-sitter.

   Adding a new entry to the list via customize will
   automatically update tree-sitter-major-mode-language-alist to
   remove the mode from the mapping, but _removing_ an entry
   requires a restart -- or at least a reset of
   tree-sitter-major-mode-language-alist. "
  :type '(repeat symbol)
  :set (lambda (symbol value)
         (set-default-toplevel-value symbol value)
         (kzk/tree-sitter-remove-verboten-languages))
  )
