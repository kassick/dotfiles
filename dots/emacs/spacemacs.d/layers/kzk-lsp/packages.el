(defconst kzk-lsp-packages
  '(general
    lsp-mode
    lsp-ui))


(defun kzk-lsp/post-init-general ())


(defun kzk-lsp/post-init-lsp-mode ()
  (general-define-key :keymaps 'lsp-mode-map
                      "C-c C-h" '("Doc Glance" . lsp-ui-doc-glance)
                      "C-c h" '("Help (no focus)" . kzk/lsp-help-at-point))

  ;; (message "setting lsp-mode minor mode == from kzk-lsp")
  ;; (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
  ;;   "==" #'lsp-format-buffer)
  ;; (define-key spacemacs-lsp-mode-map (kbd "==") 'lsp-format-buffer)
  ;;(spacemacs/set-leader-keys-for-minor-mode 'lsp-mode "==" 'lsp-format-buffer)
  (advice-add 'lsp-warn :around #'kzk/lsp-warn-advice)
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]worktrees\\'" t)

    (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
      "FC" 'kzk/lsp-workspaces-gc)))

(defun kzk-lsp/post-init-lsp-ui ()
  (advice-add #'lsp-ui-peek--peek-new :around #'kzk/lsp-ui-peek--peek-display)
  (advice-add #'lsp-ui-peek--peek-hide :around #'kzk/lsp-ui-peek--peek-destroy)

  (with-eval-after-load 'lsp-mode
    (define-key lsp-command-map (kbd "T i") #'kzk/lsp-toggle-inlay-hints))

  (unless lsp-use-upstream-bindings
    (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
      "Tli" #'kzk/lsp-toggle-inlay-hints)))
