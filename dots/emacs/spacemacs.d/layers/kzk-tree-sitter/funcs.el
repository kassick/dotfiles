(defun kzk/tree-sitter-remove-verboten-languages ()
  (with-eval-after-load 'tree-sitter-langs
    ;; tree-sitter-langs is actually responsible for populating the alist (defined in tree-sitter),
    ;; so we should wait for the fully-populated list to be available before removing entries
    (setq tree-sitter-major-mode-language-alist
          (cl-remove-if
           (lambda (e) (member (car e) kzk/tree-sitter-verboten-modes))
           tree-sitter-major-mode-language-alist))))

;; (defun kzk/tree-sit-langs--init-major-mode-alist (orig-fun &rest args)
;;   (apply orig-fun args)
;;   (kzk/tree-sitter-remove-verboten-languages)
;;   nil)
