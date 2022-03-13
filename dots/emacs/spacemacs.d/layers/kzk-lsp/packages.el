(defconst kzk-lsp-packages
  '(general
    lsp-mode))


(defun kzk-lsp/post-init-general ())


(defun kzk-lsp/post-init-lsp-mode ()
  (general-define-key :keymaps 'lsp-mode-map
                      "C-c C-h" 'lsp-ui-doc-glance
                      "C-c h" 'kzk/lsp-help-at-point))
